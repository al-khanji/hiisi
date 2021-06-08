(in-package #:hiisi/x86-64)

(defparameter *assembler-output* nil)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(define-constant +instruction-flags+
    '((:IGEN
       (:|SM|                "Size match")
       (:|SM2|               "Size match first two operands")
       (:|SB|                "Unsized operands can't be non-byte")
       (:|SW|                "Unsized operands can't be non-word")
       (:|SD|                "Unsized operands can't be non-dword")
       (:|SQ|                "Unsized operands can't be non-qword")
       (:|SO|                "Unsized operands can't be non-oword")
       (:|SY|                "Unsized operands can't be non-yword")
       (:|SZ|                "Unsized operands can't be non-zword")
       (:|SIZE|              "Unsized operands must match the bitsize")
       (:|SX|                "Unsized operands not allowed")
       (:|ANYSIZE|           "Ignore operand size even if explicit")
       (:|AR0|               "SB SW SD applies to argument 0")
       (:|AR1|               "SB SW SD applies to argument 1")
       (:|AR2|               "SB SW SD applies to argument 2")
       (:|AR3|               "SB SW SD applies to argument 3")
       (:|AR4|               "SB SW SD applies to argument 4")
       (:|OPT|               "Optimizing assembly only"))
      (:FEATURE
       (:|PRIV|              "Privileged instruction")
       (:|SMM|               "Only valid in SMM")
       (:|PROT|              "Protected mode only")
       (:|LOCK|              "Lockable if operand 0 is memory")
       (:|NOLONG|            "Not available in long mode")
       (:|LONG|              "Long mode")
       (:|NOHLE|             "HLE prefixes forbidden")
       (:|MIB|               "split base/index EA")
       (:|SIB|               "SIB encoding required")
       (:|BND|               "BND (0xF2) prefix available")
       (:|UNDOC|             "Undocumented")
       (:|HLE|               "HLE prefixed")
       (:|FPU|               "FPU")
       (:|MMX|               "MMX")
       (:|3DNOW|             "3DNow!")
       (:|SSE|               "SSE (KNI MMX2)")
       (:|SSE2|              "SSE2")
       (:|SSE3|              "SSE3 (PNI)")
       (:|VMX|               "VMX")
       (:|SSSE3|             "SSSE3")
       (:|SSE4A|             "AMD SSE4a")
       (:|SSE41|             "SSE4.1")
       (:|SSE42|             "SSE4.2")
       (:|SSE5|              "SSE5")
       (:|AVX|               "AVX  (256-bit floating point)")
       (:|AVX2|              "AVX2 (256-bit integer)")
       (:|FMA|               "")
       (:|BMI1|              "")
       (:|BMI2|              "")
       (:|TBM|               "")
       (:|RTM|               "")
       (:|INVPCID|           "")
       (:|AVX512|            "AVX-512F (512-bit base architecture)")
       (:|AVX512CD|          "AVX-512 Conflict Detection")
       (:|AVX512ER|          "AVX-512 Exponential and Reciprocal")
       (:|AVX512PF|          "AVX-512 Prefetch")
       (:|MPX|               "MPX")
       (:|SHA|               "SHA")
       (:|PREFETCHWT1|       "PREFETCHWT1")
       (:|AVX512VL|          "AVX-512 Vector Length Orthogonality")
       (:|AVX512DQ|          "AVX-512 Dword and Qword")
       (:|AVX512BW|          "AVX-512 Byte and Word")
       (:|AVX512IFMA|        "AVX-512 IFMA instructions")
       (:|AVX512VBMI|        "AVX-512 VBMI instructions")
       (:|AES|               "AES instructions")
       (:|VAES|              "AES AVX instructions")
       (:|VPCLMULQDQ|        "AVX Carryless Multiplication")
       (:|GFNI|              "Galois Field instructions")
       (:|AVX512VBMI2|       "AVX-512 VBMI2 instructions")
       (:|AVX512VNNI|        "AVX-512 VNNI instructions")
       (:|AVX512BITALG|      "AVX-512 Bit Algorithm instructions")
       (:|AVX512VPOPCNTDQ|   "AVX-512 VPOPCNTD/VPOPCNTQ")
       (:|AVX5124FMAPS|      "AVX-512 4-iteration multiply-add")
       (:|AVX5124VNNIW|      "AVX-512 4-iteration dot product")
       (:|SGX|               "Intel Software Guard Extensions (SGX)")
       (:|CET|               "Intel Control-Flow Enforcement Technology (CET)")
       (:|ENQCMD|            "Enqueue command instructions")
       (:|PCONFIG|           "Platform configuration instruction")
       (:|WBNOINVD|          "Writeback and do not invalidate instruction")
       (:|TSXLDTRK|          "TSX suspend load address tracking")
       (:|SERIALIZE|         "SERIALIZE instruction")
       (:|AVX512BF16|        "AVX-512 bfloat16")
       (:|AVX512VP2INTERSECT| "AVX-512 VP2INTERSECT instructions")
       (:|AMXTILE|           "AMX tile configuration instructions")
       (:|AMXBF16|           "AMX bfloat16 multiplication")
       (:|AMXINT8|           "AMX 8-bit integer multiplication")
       (:|OBSOLETE|          "Instruction removed from architecture")
       (:|NEVER|             "Instruction never implemented")
       (:|NOP|               "Instruction is always a (nonintentional) NOP")
       (:|VEX|               "VEX or XOP encoded instruction")
       (:|EVEX|              "EVEX encoded instruction"))
      (:CPU
       (:|8086|              "8086")
       (:|186|               "186+")
       (:|286|               "286+")
       (:|386|               "386+")
       (:|486|               "486+")
       (:|PENT|              "Pentium")
       (:|P6|                "P6")
       (:|KATMAI|            "Katmai")
       (:|WILLAMETTE|        "Willamette")
       (:|PRESCOTT|          "Prescott")
       (:|X86_64|            "x86-64 (long or legacy mode)")
       (:|NEHALEM|           "Nehalem")
       (:|WESTMERE|          "Westmere")
       (:|SANDYBRIDGE|       "Sandy Bridge")
       (:|FUTURE|            "Ivy Bridge or newer")
       (:|IA64|              "IA64 (in x86 mode)")
       (:|ANY|               "Any x86 CPU")
       (:|CYRIX|             "Cyrix-specific")
       (:|AMD|               "AMD-specific"))))

(define-constant +bit-sizes+
    '((:byte 8)
      (:word 16)
      (:dword 32)
      (:qword 64)
      (:tword 80)
      (:oword 128)
      (:yword 256)
      (:zword 512)))

(define-constant +legacy-prefixes+
    '((#x66)
      (#x67)
      (#x2E :cS)
      (#x3E :ds)
      (#x26 :es)
      (#x64 :fs)
      (#x65 :gs)
      (#x36 :ss)
      (#xF0 :lock)
      (#xF3 :rep :repe :repz)
      (#xF2 :repne :repnz)))

(define-constant +rex+ #b01000000)
(define-constant +rex.w+ (logior +rex+ #b1000))
(define-constant +rex.r+ (logior +rex+ #b0100))
(define-constant +rex.x+ (logior +rex+ #b0010))
(define-constant +rex.b+ (logior +rex+ #b0001))

(defun generate-prefixes (&optional (static-prefixes '("0F24" "0F25" "0F38" "0F3A" "0F7A" "0FA6" "0FA7" "0F"))
                                    (prefixes '("vex" "xop" "evex"))
                                    (m-count 32)
                                    (p-count 4))
  (let (output)
    (dolist (prefix prefixes)
      (dotimes (m m-count)
        (dotimes (p p-count)
          (push (format nil "~A~2,'0X~1,'0X" prefix m p) output))))
    (append static-prefixes (nreverse output))))

(defun legacy-prefix-p (x)
  (find x +legacy-prefixes+ :key #'car))

(defun rex-prefix-p (x)
  (when (= +rex+ (logand +rex+ x))
    x))

(define-constant +imm-codes+
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

(define-constant +plain-codes+
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

(defun plain-code-p (op)
  (find op +plain-codes+ :test #'string=))

(defun encoding->code (codestr)
  (cl-ppcre:register-groups-bind (dummy opr tuple opc)
      ("^(([^\\s:]*)\\:*([^\\s:]*)\\:|)\\s*(.*\\S)\\s*$" codestr)
    (declare (ignore dummy))
    (let* ((oppos (loop with op = 0
                        for c across (or opr #())
                        for plusp = (char= #\+ c)
                        unless plusp collect (cons c op)
                        if plusp do (decf op)
                        else do (incf op)))
           (ops (cl-ppcre:split "\\s*(?:\\s|(?=[\\/\\\\]))" opc))
           (last-imm "h")
           (prefix-ok t)
           code
           last-match-regs)
      (dolist (op ops)
        (cond ((plain-code-p op)
               (push op code))
              ((and prefix-ok (find op '("66" "f2" "f3") :test #'string=))
               (push op code))
              ((cl-ppcre:scan "^[0-9a-f]{2}$" op)
               (push op code))
              ((string= op "/r")
               (let* ((r-pos (cdr (assoc #\r oppos)))
                      (m-pos (cdr (assoc #\m oppos)))
                      (opex (logior (if (logand r-pos 4) 5 0)
                                    (if (logand m-pos 4) 6 0))))
                 (unless (and r-pos m-pos)
                   (error (concatenate 'string codestr ": /r requires r and m operands")))
                 (when (/= opex 0)
                   (push (list :opex opex) code))
                 (push (list :modrm
                             100
                             (list m-pos (logand m-pos 3))
                             (list r-pos (logand r-pos 3)))
                       code)
                 (setf prefix-ok nil)))
              ((multiple-value-bind (match regs) (cl-ppcre:scan-to-strings "^/([0-7])$" op)
                 (setf last-match-regs regs)
                 match)
               (let ((m-pos (cdr (assoc #\m oppos))))
                 (unless m-pos
                   (error (concatenate 'string codestr ": " op "requires an m operand")))
                 (when (logand m-pos 4)
                   (push (list :opex #o6) code))
                 (push `(:modrm 200
                                ,m-pos
                                ,last-match-regs)
                       code)))
              (t
               (push (list :fallback op) code))))
      (list oppos tuple ops (nreverse code)))))

(defun encoding->assembler (encoding)
  (when (string= encoding "ignore")
    (return-from encoding->assembler nil))
  (cl-ppcre:register-groups-bind (first)
      ("^\\s*\\[([^\\]]*)\\]\\s*$" encoding)
    (return-from encoding->assembler (encoding->code first)))
  (error encoding "unknown code format"))

(defun make-assembler (name operands encoding flags)
  (list name operands (encoding->assembler encoding) flags))

(defun frob-assembler (insns)
  (format nil "~{~W~%~W~^~%~%~}" (loop for x in insns
                                     for y = (apply #'make-assembler x)
                                     collect x
                                     collect y)))

(labels ((whitespace-p (x &optional (whitespace '(#\Space #\Tab)))
           (find x whitespace))
         (read-word (stream &optional (test #'whitespace-p) (skip-whitespace-p t))
           (when skip-whitespace-p
             (peek-char t stream))
           (loop for c = (peek-char nil stream nil nil)
                 while (and c (not (funcall test c)))
                 collect (read-char stream) into chars
                 finally (return (coerce chars 'string))))
         (split-if (predicate sequence &key (start 0) end remove-empty-subseqs)
           (loop with end = (or end (length sequence))
                 for left = start then (1+ right)
                 for right = (position-if predicate sequence :start left :end end)
                 for subseq-end = (or right end)
                 if (or (plusp (- subseq-end left))
                        (not remove-empty-subseqs))
                 collect (subseq sequence left (or right end))
                 while right))
         (split (needle sequence &key (start 0) end remove-empty-subseqs)
           (split-if (lambda (c) (eql needle c))
                     sequence
                     :start start
                     :end end
                     :remove-empty-subseqs remove-empty-subseqs))
         (make-keyword (s)
           (intern s :keyword))
         (make-keywords (seq)
           (map-into seq #'make-keyword seq))
         (split-keywords (str &optional (needle #\,))
           (make-keywords (split needle str)))
         (split-encoding (encoding)
           (let ((left 0)
                 (right (1- (length encoding))))
             (if (and (char= #\[ (elt encoding left))
                      (char= #\] (elt encoding right)))
                 (split-if #'whitespace-p
                           encoding
                           :start (1+ left)
                           :end right
                           :remove-empty-subseqs t)
                 encoding)))
         (parse-operand (operand)
           ((lambda (x)
              (if (cdr x) x (car x)))
            (split-keywords operand #\|)))
         (split-operands (operands)
           (mapcar #'parse-operand
                   (split #\, operands)))
         (explode-reg-names (name)
           (let ((dash-pos (position #\- name)))
             (if dash-pos
                 (loop with first-digit-pos = (position-if #'digit-char-p name)
                       with last-digit-pos = (position-if #'digit-char-p name :from-end t)
                       with prefix =(subseq name 0 first-digit-pos)
                       with suffix = (subseq name (1+ last-digit-pos))
                       with start-number = (read-from-string name t nil
                                                             :start first-digit-pos
                                                             :end dash-pos)
                       with end-number = (read-from-string name t nil
                                                           :start (1+ dash-pos)
                                                           :end (1+ last-digit-pos))
                       for i from start-number to end-number
                       collect (format nil "~A~A~A" prefix i suffix))
                 (list name))))
         (skip-line-p (line comment-char)
           (with-input-from-string (in line)
             (char= comment-char (peek-char t in nil comment-char))))
         (parse-reg-line (line)
           (with-input-from-string (in (string-upcase line))
             (let* ((name (read-word in))
                    (assembler-class (read-word in))
                    (disassembler-classes (read-word in))
                    (register-number (read in)))
               (loop for name in (explode-reg-names name)
                     for num from register-number
                     collect (list (make-keyword name)
                                   (make-keyword assembler-class)
                                   (split-keywords disassembler-classes)
                                   num)))))
         (parse-insn-line (line)
           (with-input-from-string (in line)
             (let* ((name (read-word in))
                    (operands (read-word in))
                    (encoding (case (peek-char t in)
                                (#\[ (read-word in
                                                (let (previous-char)
                                                  (lambda (current-char)
                                                    (prog1 (eql previous-char #\])
                                                      (setf previous-char current-char))))))
                                (t (read-word in))))
                    (flags (read-word in)))
               (list (make-keyword name)
                     (unless (or (string= "ignore" operands)
                                 (string= "void" operands))
                       (split-operands (string-upcase operands)))
                     encoding
                     (unless (string= "ignore" flags)
                       (split-keywords flags)))))))
  (defun slurp-regs (filepath)
    (with-open-file (in filepath)
      (loop for line = (read-line in nil nil)
            while line
            unless (skip-line-p line #\#)
            append (parse-reg-line line))))
  (defun slurp-insns (filepath)
    (with-open-file (in filepath)
      (loop for line = (read-line in nil nil)
            while line
            unless (skip-line-p line #\;)
            collect (parse-insn-line line)))))
