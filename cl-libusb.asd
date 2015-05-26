;;;; cl-libusb.asd

(asdf:defsystem #:cl-libusb
  :name "CL-libusb"
  :description "Lispified bindings to libusb-0.1."
  :author "Sumant Oemrawsingh"
  :serial t
  :depends-on (#:libusb-ffi #:trivial-garbage)
  :components ((:file "package")
               (:file "cl-libusb")))

