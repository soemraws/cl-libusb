;;;; package.lisp

(defpackage #:libusb-ffi
  (:use #:cl #:cffi)
  (:export #:usb-get-busses
	   #:usb-get-devices
	   #:usb-get-devices-by-ids

	   #:usb-open
	   #:usb-close

	   #:usb-get-vendor-id
	   #:usb-get-product-id

	   #:usb-device-get-descriptor
	   #:usb-get-configurations
	   #:usb-get-configuration-by-value
	   #:usb-configuration-get-value
	   #:usb-configuration-get-interfaces
	   #:usb-interface-get-settings
	   #:usb-interface-setting-get-number
	   #:usb-interface-setting-get-alternate
	   #:usb-interface-setting-get-endpoints
	   #:usb-endpoint-get-address
	   
	   #:usb-get-string-index
	   #:usb-get-string
	   #:usb-bulk-read
	   #:usb-bulk-write
	   #:usb-interrupt-read
	   #:usb-interrupt-write
           #:usb-control-msg

	   #:usb-claim-interface
	   #:usb-release-interface
	   #:usb-set-configuration
	   #:usb-set-altinterface
	   
	   #:usb-clear-halt
	   #:usb-reset

	   #:endpoint-in-p
	   #:endpoint-out-p))


(defpackage #:cl-libusb
  (:use #:cl)
  (:export #:usb-device
	   #:usb-get-devices-by-ids

	   #:usb-open
	   #:usb-close
	   #:usb-open-p

	   #:usb-bulk-read
	   #:usb-bulk-write
	   #:usb-interrupt-read
	   #:usb-interrupt-write
           #:usb-control-msg

	   #:usb-get-vendor-id
	   #:usb-get-product-id
	   #:usb-get-string

	   #:usb-claim-interface
	   #:usb-release-interface
	   #:usb-set-configuration
	   #:usb-set-alt-interface
	   #:usb-simple-setup

	   #:usb-clear-halt))
