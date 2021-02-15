;;;; cl-libusb.lisp

(in-package #:cl-libusb)

(defclass usb-device ()
  ((device-pointer :initarg :device-pointer :reader usb-device-pointer)
   (handle-pointer :initform nil :reader usb-handle-pointer)
   (claimed-interfaces :initform nil)))

(defun usb-open-p (device)
  "Predicate to see if a device has been opened."
  (not (null (usb-handle-pointer device))))

(defun usb-open (device)
  "Open a usb device. If the device is already open, do nothing."
  (unless (usb-open-p device)
    (let ((handle (libusb-ffi:usb-open (usb-device-pointer device))))
      (setf (slot-value device 'handle-pointer) handle)
      (tg:finalize device #'(lambda () (libusb-ffi:usb-close handle)))))
  device)

(defun usb-close (device)
  "Close a usb device. If a device is already closed, do nothing."
  (when (usb-open-p device)
    (let ((handle (usb-handle-pointer device)))
      (dolist (number (slot-value device 'claimed-interfaces))
	(libusb-ffi:usb-release-interface handle number))
      (setf (slot-value device 'claimed-interfaces) nil)
      (libusb-ffi:usb-close handle)
      (setf (slot-value device 'handle-pointer) nil)
      (tg:cancel-finalization device)))
  device)

(defun usb-get-vendor-id (device)
  "Return the vendor id of the device."
  (libusb-ffi:usb-get-vendor-id (usb-device-pointer device)))

(defun usb-get-product-id (device)
  "Return the product id of the device."
  (libusb-ffi:usb-get-product-id (usb-device-pointer device)))

(defun usb-get-devices-by-ids (vendor-id product-id)
  "Return a list of devices that match the given vendor id and product
  id. If either is NIL, that parameter is not used as a filter. Thus
  if both are NIL, a list containing all devices is returned."
  (flet ((make-device-from-pointer (pointer)
	   (make-instance 'usb-device :device-pointer pointer)))
    (mapcar #'make-device-from-pointer
	    (libusb-ffi:usb-get-devices-by-ids vendor-id product-id))))

(defun usb-get-string (device index-or-symbol &optional language-id)
  "Return the string associated with the given index or symbol. If no
  language id is given, a simple ascii string is returned, else the
  string with the given language id is returned. The allowed symbols
  are :manufacturer, :product or :serial-number. If the device was not
  open, it is opened to obtain the string and then closed again."
  (let ((index index-or-symbol)
	(was-open (usb-open-p device)))
    (unless (integerp index)
      (setf index (libusb-ffi:usb-get-string-index (usb-device-pointer device)
						   index-or-symbol)))
    (unless was-open
      (usb-open device))
    (unwind-protect
	 (libusb-ffi:usb-get-string (usb-handle-pointer device)
				    index
				    language-id)
      (unless was-open
	(usb-close device)))))

(defun usb-claim-interface (device setting-or-number)
  "Claim the given interface for the handle. The interface can be
  specified by its setting, or its (integer) number."
  (let ((number (if (integerp setting-or-number)
		    setting-or-number
		    (libusb-ffi:usb-interface-setting-get-number setting-or-number)))
	handle)
    (with-slots (claimed-interfaces handle-pointer) device
      (unless (find number claimed-interfaces
		    :test #'=)
	(usb-open device)
	(setf handle handle-pointer)
	(libusb-ffi:usb-claim-interface handle number)
	(push number claimed-interfaces)
	(tg:finalize device
		     #'(lambda ()
			 (libusb-ffi:usb-release-interface handle number)))))))

(defun usb-rebuild-finalization (device)
  "Rebuild the finalization list for the given USB device."
  (tg:cancel-finalization device)
  (if (usb-open-p device)
      (let ((handle (usb-handle-pointer device)))
	(tg:finalize device
		     #'(lambda ()
			 (libusb-ffi:usb-close handle)))
	(dolist (number (slot-value device 'claimed-interfaces))
	  (tg:finalize device
		       #'(lambda ()
			   (libusb-ffi:usb-release-interface handle number)))))
      (setf (slot-value device 'claimed-interfaces) nil)))

(defun usb-release-interface (device setting-or-number)
  "Release the given interface for the handle. The interface can be
  specified by its setting, or its (integer) number."
  (let ((number (if (integerp setting-or-number)
		    setting-or-number
		    (libusb-ffi:usb-interface-setting-get-number setting-or-number))))
    (with-slots (claimed-interfaces handle-pointer) device
      (when (find number claimed-interfaces
		  :test #'=)
	(libusb-ffi:usb-release-interface handle-pointer number)
	(setf claimed-interfaces (delete number claimed-interfaces :test #'=))
	(usb-rebuild-finalization device)))))

(defun usb-set-altinterface (device setting-or-number)
  "Set the alternate interface setting to that of the given
  setting. The alternate interface setting can be specified by
  setting, or by its (integer) value."
  (libusb-ffi:usb-set-altinterface (usb-handle-pointer device)
				   setting-or-number))

(defun usb-get-configuration (device)
  "Returns the current configuration value, or -1 if no configuration is set."
  (let ((current-config (cffi:foreign-slot-value (usb-device-pointer device) '(:struct libusb-ffi::device) 'libusb-ffi::configuration)))
    (if (cffi:null-pointer-p current-config)
	-1
	(cffi:foreign-slot-value current-config '(:struct libusb-ffi::configuration) 'libusb-ffi::configuration-value))))

(defun usb-set-configuration (device configuration-or-number)
  "Set the given configuration for the handle. The configuration can
  be specified also by its (integer) value."
  (let ((number (if (cffi:pointerp configuration-or-number)
		    (libusb-ffi::usb-configuration-get-value configuration-or-number)
		    configuration-or-number)))
    (if (= (usb-get-configuration device) number)
	(libusb-ffi:usb-set-configuration (usb-handle-pointer device) number)
	0)))

(defun usb-simple-setup (device)
  "Set up the device by using the first found configuration, interface
  and settings."
  (usb-open device)
  (let* ((configuration
	  (car (libusb-ffi:usb-get-configurations (usb-device-pointer device))))
	 (interface
	  (car (libusb-ffi:usb-configuration-get-interfaces configuration)))
	 (setting
	  (car (libusb-ffi:usb-interface-get-settings interface))))
    (usb-set-configuration device configuration)
    (usb-claim-interface device setting)
    (usb-set-altinterface device setting)))

(defun usb-get-driver-name (dev)
  (libusb-ffi:usb-get-driver-name (usb-handle-pointer dev)))

(defun usb-detach-kernel-driver-np (dev interface)
  (libusb-ffi:usb-detach-kernel-driver-np* (usb-handle-pointer dev) interface))

(defun usb-bulk-read (device endpoint bytes-to-read timeout)
  "Read the given amount of bytes in a bulk transfer and return the
  buffer (a static vector)."
  (libusb-ffi:usb-bulk-read (usb-handle-pointer device)
			    endpoint bytes-to-read timeout))

(defun usb-bulk-write (device endpoint buffer timeout)
  "Write data in the given buffer (a static vector) in a bulk transfer
  and return the amount of bytes actually written."
  (libusb-ffi:usb-bulk-write (usb-handle-pointer device)
			    endpoint buffer timeout))

(defun usb-interrupt-read (device endpoint bytes-to-read timeout)
  "Read the given amount of bytes in an interrupt transfer and return
  the buffer (a static vector)."
  (libusb-ffi:usb-interrupt-read (usb-handle-pointer device)
			    endpoint bytes-to-read timeout))

(defun usb-interrupt-write (device endpoint buffer timeout)
  "Write data in the given buffer (a static vector) in an interrupt
  transfer and return the amount of bytes actually written."
  (libusb-ffi:usb-interrupt-write (usb-handle-pointer device)
			    endpoint buffer timeout))

(defun usb-control-msg (device requesttype request value index buffer timeout)
  (libusb-ffi:usb-control-msg (usb-handle-pointer device) requesttype request value index buffer timeout))

(defun usb-clear-halt (device endpoint)
  "Clear the halt flag on the given endpoint of the device."
  (libusb-ffi:usb-clear-halt (usb-handle-pointer device) endpoint))
