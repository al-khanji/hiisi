(asdf:defsystem hiisi
  :components ((:file "packages")
               (:file "hiisi-x86" :depends-on ("packages")))
  :depends-on (:cl-ppcre))
