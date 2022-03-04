(in-package #:hiisi/x86)

(define-condition bad-line (error)
  ((filename :initarg :filename
             :initform nil
             :reader filename)
   (line-number :initarg :line-number
                :initform nil
                :reader line-number)
   (line :initarg :line
         :initform nil
         :reader line))
  (:documentation "Encountered a malformed line in a file")
  (:report (lambda (condition stream)
             (format stream "~a:~a malformed line: ~a~&" (filename condition) (line-number condition) (line condition)))))

(defstruct register
  name
  assembler-class
  disassembler-classes
  register-number
  token-flag)

(defparameter *registers* nil
  "Known registers")

(defparameter *register-assembler-classes*
  '(BNDREG
    FPU0
    FPUREG
    MMXREG
    OPMASK0
    OPMASKREG
    REG16NA
    REG32NA
    REG64NA
    REG8NA
    REG_AL
    REG_AX
    REG_CL
    REG_CREG
    REG_CS
    REG_CX
    REG_DL
    REG_DREG
    REG_DS
    REG_DX
    REG_EAX
    REG_ECX
    REG_EDX
    REG_ES
    REG_FS
    REG_GS
    REG_HIGH
    REG_RAX
    REG_RCX
    REG_RDX
    REG_SEG67
    REG_SS
    REG_TREG
    TMMREG
    XMM0
    XMMREG
    XMM_L16
    YMM0
    YMMREG
    YMM_L16
    ZMM0
    ZMMREG
    ZMM_L16))

(defun add-register (name assembler-class disassembler-classes register-number token-flag)
  "Add a new register to *REGISTERS*."
  (push (make-register :name name
                       :assembler-class assembler-class
                       :disassembler-classes disassembler-classes
                       :register-number register-number
                       :token-flag token-flag)
        *registers*))

(defun register-names (reg)
  "Given a name like \"r8-15d\" expands it into a list of names."
  (multiple-value-bind (match strings)
      (cl-ppcre:scan-to-strings "^(.*[^0-9])([0-9]+)\\-([0-9]+)(|[^0-9].*)$" reg)
    (unless match
      (return-from register-names (list reg)))
    (let ((prefix (aref strings 0))
          (suffix (aref strings 3))
          (nregs (1+ (- (parse-integer (aref strings 2))
                        (parse-integer (aref strings 1))))))
      (loop repeat nregs
            for reg-number from (parse-integer (aref strings 1))
            collect (format nil "~a~a~a" prefix reg-number suffix)))))

(defun process-regs.dat-line (line-number reg aclass dclasses x86regno &optional token-flag)
  "Process a single data line from regs.dat. Insert results into *REGISTERS*."
  (declare (ignore line-number))
  (let ((aclass (read-from-string aclass))
        (dclasses (mapcar #'read-from-string (cl-ppcre:split "," dclasses)))
        (x86regno (parse-integer x86regno :radix (if (cl-ppcre:scan "^0" x86regno)
                                                     8
                                                     10)))
        (token-flag (read-from-string token-flag nil nil)))
    (loop for reg in (register-names reg)
          for num from x86regno
          do (add-register (read-from-string reg) aclass dclasses num token-flag))))

(defun slurp-lines (filename &key (filter (constantly nil)))
  "Read lines from FILENAME and return a list of `(line-number line-text)'. A per-line :filter
may be supplied, given the line as input return non-NIL to skip it."
  (with-open-file (f filename)
    (loop for n from 1
          for line = (read-line f nil nil)
          while line
          unless (funcall filter line)
            collect (list n line))))

(defun process-regs.dat (filename)
  "Read register info from regs.dat found at FILENAME and insert into *REGISTERS*."
  (loop for (line-number line)
          in (slurp-lines filename
                          :filter (lambda (line)
                                    ; skip comments or blank lines
                                    (string= "" (cl-ppcre:regex-replace "\\s*(\\#.*|)$" line ""))))
        with scanner
          = (cl-ppcre:create-scanner "\\s*(\\S+)\\s*(\\S+)\\s*(\\S+)\\s*([0-9]+)\\s*(\\S*)"
                                     :case-insensitive-mode t)
        do (multiple-value-bind (match strings) (cl-ppcre:scan-to-strings scanner line)
             (unless match
               (error 'bad-line :filename filename :line-number line-number :line line))
             (apply #'process-regs.dat-line line-number (coerce strings 'list)))))

(defparameter +max-operands+ 5)

(defparameter *instructions* nil)

(defstruct instruction
  name
  operands
  code
  flags)

(defun instruction-forms (name operands code flags)
  (flet ((relaxed-op-p (op) (cl-ppcre:scan "\\*$" op)))
    (let* ((ops (cl-ppcre:split "," operands))
           (n-ops (length ops))
           (has-relaxed-forms (some #'relaxed-op-p ops))
;           (name (intern name))
           (flags (mapcar #'intern (cl-ppcre:split "," flags))))
      (append (list (list name (join-strings "," ops) code flags 0))
              (when has-relaxed-forms
                (when (relaxed-op-p (first ops))
                  (error "instruction ~A has first operand with a *" name))
                (loop with opmask = (apply #'logior (loop for n from 0
                                                          for op in ops
                                                          when (relaxed-op-p op)
                                                            collect (ash 1 n)))
                      for oi from 1 below (ash 1 n-ops)
                      when (zerop (logandc2 oi opmask))
                        collect (list name
                                      (join-strings ","
                                                    (loop for op in ops
                                                          for omask = (lognot oi) then (ash omask -1)
                                                          unless (zerop (logand omask 1))
                                                            collect op))
                                      code
                                      flags
                                      oi)))))))

(defun join-strings (delimiter strings)
  (let ((format-string (format nil "~~{~~A~~^~A~~}" delimiter)))
    (format nil format-string strings)))

(defun do-replacements (target-string replacements &key (replace-all t))
  (let ((fn (if replace-all
                #'cl-ppcre:regex-replace-all
                #'cl-ppcre:regex-replace)))
    (loop for (regex replacement) in replacements
          do (setf target-string (funcall fn regex target-string replacement)))
    target-string))

(defun format-operand (operand evex-p)
  (let (opsz opx opevex)
    (dolist (opp (cl-ppcre:split "\\|" operand))
      (setf opp (cl-ppcre:regex-replace "^(b(32|64)|mask|z|er|sae)$"
                                        opp
                                        #'(lambda (match reg0 &rest remaining-registers)
                                            (declare (ignore match remaining-registers))
                                            (push reg0 opevex)
                                            "")
                                        :simple-calls t))
      (setf opp (cl-ppcre:regex-replace "(?<!\\d)(8|16|32|64|80|128|256|512)$"
                                        opp
                                        #'(lambda (match reg0 &rest remaining-registers)
                                            (declare (ignore match remaining-registers))
                                            (setf opsz (parse-integer reg0 :radix 10))
                                            "")
                                        :simple-calls t))
      (setf opp (do-replacements opp (append '(("^mem$" "memory")
                                               ("^memory_offs$" "mem_offs")
                                               ("^imm$" "immediate")
                                               ("^([a-z]+)rm$" "rm_\\{1}")
                                               ("^rm$" "rm_gpr")
                                               ("^reg" "reg_gpr"))
                                             (unless evex-p
                                               '(("^(rm_[xyz]mm)$" "\\{1}_l16")
                                                 ("^([xyz]mm)reg$" "\\{1}_l16"))))))
      (unless (string= opp "")
        (push opp opx)))
    (flet ((symbolicate (list) (reverse (mapcar #'read-from-string list))))
      (list :opx (symbolicate opx)
            :opsz opsz
            :opevex (symbolicate opevex)))))

(defparameter *imm-codes*
  '("ib"
    "ib,u"
    "iw"
    "ib,s"
    "iwd"
    "id"
    "id,s"
    "iwdq"
    "rel8"
    "iq"
    "rel16"
    "rel"
    "rel32"
    "seg"))

(defparameter *plain-codes*
  '("o16"
    "o32"
    "odf"
    "o64"
    "o64nw"
    "a16"
    "a32"
    "adf"
    "a64"
    "!osp"
    "!asp"
    "f2i"
    "f3i"
    "mustrep"
    "mustrepne"
    "rex.l"
    "norexb"
    "norexx"
    "norexr"
    "norexw"
    "repe"
    "nohi"
    "nof3"
    "norep"
    "wait"
    "resb"
    "np"
    "jcc8"
    "jmp8"
    "jlen"
    "hlexr"
    "hlenl"
    "hle"
    "vsibx"
    "vm32x"
    "vm64x"
    "vsiby"
    "vm32y"
    "vm64y"
    "vsibz"
    "vm32z"
    "vm64z"))

(defun imm-code-p (op)
  (member op *imm-codes*))

(defun plain-code-p (op)
  (member op *plain-codes*))

(defun instruction-code-parse (code)
  (assert (cl-ppcre:scan "^\\s*\\[([^\\]]*)\\]\\s*$" code))
  (multiple-value-bind (match fields) (cl-ppcre:scan-to-strings "^(([^\\s:]*)\\:*([^\\s:]*)\\:|)\\s*(.*\\S)\\s*$"
                                                                (subseq code 1 (1- (length code))))
    (unless match
      (error "Can't parse ~A" code))
    (mapcar #'string-downcase (coerce (subseq fields 1 4) 'list))))

(defun instruction-code-op-positions (opr relax)
  (loop for op from 0
        for c across opr
        for plus-p = (char= #\+ c)
        for relax-p = (= 1 (logand relax 1))
        when (or plus-p relax-p)
          do (decf op)
        unless plus-p
          collect (cons c op) into oppos
          and do (setf relax (ash relax -1))
        finally (return oppos)))

(defun instruction-code-compile (code relax)
  (when (string= code "ignore")
    (return-from instruction-code-compile))

  (loop with (opr tuple opc) = (instruction-code-parse code)
        with ops = (cl-ppcre:split "\\s*(?:\\s|(?=[\\/\\\\]))" opc)
        with oppos = (instruction-code-op-positions opr relax)
        with last-immediate = "h"
        with prefix-ok = t
        with litix
        with output
        for op in ops
        do (cond
             ;; plain code
             ((plain-code-p op)
              (push op output))
             ;; prefix
             ((and prefix-ok (member op '("66" "f2" "f3")))
              (push op output))
             ;; literal
             ((cl-ppcre:scan "^[0-9a-f]{2}$" op)
              (push op output)
              (setf prefix-ok nil))
             ;; rm
             ((string= op "/r")
              (let ((r-pos (cdr (assoc #\r oppos)))
                    (m-pos (cdr (assoc #\m oppos)))
                    (x-pos (cdr (assoc #\x oppos))))
                (assert (and r-pos m-pos))
                (let ((opex (logior (if (zerop (logand r-pos 4))
                                        0
                                        5)
                                    (if (zerop (logand m-pos 4))
                                        0
                                        6))))
                  (unless (zerop opex)
                    (push `(opex ,opex) output)))
                (when x-pos
                  (push (+ #o14 (logand x-pos 3)) output))
                (push (+ #o100 (ash (logand m-pos 3) 3)) output)
                (setf prefix-ok nil)))
             (t (push `(unknown ,op) output)))
        finally (return (list oppos tuple (reverse output)))))


(defun process-instruction (name operands code flags relax)
  (let* ((evex-p (cl-ppcre:scan "(^|\\s)evex\\." code))
         (ops (unless (string= operands "void")
                (loop for operand in (cl-ppcre:split "\\," (do-replacements operands '(("\\*" "")
                                                                                       (":" "|colon,"))))
                      collect (format-operand operand evex-p)))))
    (when (null ops)
      (assert (zerop relax)))
    (push (make-instruction :name name
                            :operands ops
                            :code code
                            :flags (append flags
                                           (unless (zerop relax)
                                             `((relax ,relax)))))
          *instructions*)))

(defun process-insns.dat-line (line-number name operands code flags)
  (declare (ignore line-number))
  (loop for (name ops code flags relax) in (instruction-forms name operands code flags)
        do (process-instruction name ops code flags relax)))

(defun process-insns.dat (filename)
  (loop for (line-number line)
          in (slurp-lines filename
                          :filter (lambda (line)
                                        ; skip comments or blank lines
                                    (string= "" (cl-ppcre:regex-replace "^\\s*(\\;.*|)$" line ""))))
        with scanner
          = (cl-ppcre:create-scanner "^\\s*(\\S+)\\s+(\\S+)\\s+(\\S+|\\[.*\\])\\s+(\\S+)\\s*$")
        do (multiple-value-bind (match strings) (cl-ppcre:scan-to-strings scanner line)
             (unless match
               (error 'bad-line :filename filename :line-number line-number :line line))
             (apply #'process-insns.dat-line line-number (coerce strings 'list)))))

(defun %reload ()
  (setf *registers* nil
        *instructions* nil)
  (process-regs.dat "~/github/netwide-assembler/nasm/x86/regs.dat")
  (process-insns.dat "~/github/netwide-assembler/nasm/x86/insns.dat"))


(defparameter *test-program* '((push #x1)
                               (pop rdi)
                               (mov rax rdi)
                               (syscall)))

(defun %find-entries (name key list)
  (let ((needle (string-upcase (etypecase name
                                 (symbol (symbol-name name))
                                 (string name)))))
    (remove-if-not (lambda (i)
                     (string= needle (string-upcase (string (funcall key i)))))
                   list)))

(defun %find-instructions (name)
  (%find-entries name #'instruction-name *instructions*))

(defun %find-registers (name)
  (%find-entries name #'register-name *registers*))

(defclass value-type ()
  ((value :accessor value :initarg :value :type integer :initform (error "Must supply value"))))

(defclass immediate (value-type)
  ((width :accessor width :initarg :width :type integer :initform (error "Must supply width"))))

(defgeneric make-value-type (value)
  (:documentation "Create a value type for value"))

(defun imm-width (integer)
  (loop repeat 5
        with width = (integer-length integer)
        for imm-width from 8 by 8
        while (<= imm-width width)
        finally (return imm-width)))

(defmethod make-value-type ((n integer))
  (make-instance 'immediate :value n :width (imm-width n)))

(defgeneric %operand-and-value-type-compatible-p (operand value-type))

(defmethod %operand-and-value-type-compatible-p (operand value-type)
  (declare (ignore operand value-type))
  nil)

(defmethod %operand-and-value-type-compatible-p ((o string) (imm immediate))
  (multiple-value-bind (match fields) (cl-ppcre:scan-to-strings "imm([0-9]{1,2})" o)
    (when match
      (>= (parse-integer (aref fields 0)) (width imm)))))

(defmethod %operand-and-value-type-compatible-p ((o string) (reg (eql 'eax)))
  (let ((registers (%find-registers reg)))
    (when (= 1 (length registers))
      )
    (unless (rest registers)
      ))
  (format t "EAX BABY~%")
  t)

(defun %operand-and-parameter-compatible-p (o p)
  (%operand-and-value-type-compatible-p o (make-value-type p)))

(defun %instruction-and-parameters-compatible-p (instruction parameters)
  (let ((operands (instruction-operands instruction)))
    (when (= (length operands) (length parameters))
      (mapcar (lambda (operand parameter)
                (unless (%operand-and-parameter-compatible-p operand parameter)
                  (return-from %instruction-and-parameters-compatible-p)))
              operands parameters)
      t)))

(defun %compile-form (form)
  (loop with operation = (first form)
        with parameters = (rest form)
        for instruction in (%find-instructions operation)
        if (%instruction-and-parameters-compatible-p instruction parameters)
          collect instruction))

(defun %compile (forms)
  (loop for form in forms
        collect (list :input form :output (%compile-form form))))

(%reload)
