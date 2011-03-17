;;;; libusb-ffi.asd

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(asdf:defsystem #:libusb-ffi
  :name "libusb-ffi"
  :description "Common Lisp FFI bindings to libusb-0.1."
  :serial t
  :depends-on (#:cffi #:cffi-grovel #:grid #:foreign-array)
  :components ((:file "package")
	       (cffi-grovel:grovel-file "libusb-grovel")
               (:file "libusb-ffi")))
