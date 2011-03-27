;;;; libusb-ffi.lisp

(in-package #:libusb-ffi)

(define-foreign-library libusb
  (:unix (:or "libusb-0.1.so.4.4" "libusb-0.1.so.4" "libusb-0.1.so"))
  (:windows "libusb0")
  (t (:default "libusb")))

(use-foreign-library libusb)

;;; FFI

(defctype bus-ptr :pointer)
(defctype device-ptr :pointer)
(defctype device-handle-ptr :pointer)

(defcfun (usb-init* "usb_init") :void)

(defcfun (usb-find-busses* "usb_find_busses") :int)

(defcfun (usb-find-devices* "usb_find_devices") :int)

(defcfun (usb-get-busses* "usb_get_busses") bus-ptr)

(defcfun "usb_open" device-handle-ptr
  "Open a usb device and return a pointer to the handle to be used for
  communications."
  (device device-ptr))

(defcfun "usb_close" :void
  "Close a usb device by the pointer to its handle."
  (handle device-handle-ptr))

(defcfun (usb-get-string-simple* "usb_get_string_simple") :int
  (handle device-handle-ptr)
  (index :int)
  (buffer :pointer)
  (buffer-size :size-t))

(defcfun (usb-get-string* "usb_get_string") :int
  (handle device-handle-ptr)
  (index :int)
  (language-id :int)
  (buffer :pointer)
  (buffer-size :size-t))

(defcfun (usb-claim-interface* "usb_claim_interface") :int
  (handle device-handle-ptr)
  (interface :int))

(defcfun (usb-release-interface* "usb_release_interface") :int
  (handle device-handle-ptr)
  (interface :int))

(defcfun (usb-set-configuration* "usb_set_configuration") :int
  (handle device-handle-ptr)
  (configuration :int))

(defcfun (usb-set-altinterface* "usb_set_altinterface") :int
  (handle device-handle-ptr)
  (alternate :int))

(defcfun (usb-clear-halt* "usb_clear_halt") :int
  (handle device-handle-ptr)
  (endpoint :unsigned-int))

(defcfun (usb-reset* "usb_reset") :int
  (handle device-handle-ptr))

(defcfun (usb-bulk-write* "usb_bulk_write") :int
  (handle device-handle-ptr)
  (endpoint :int)
  (bytes :pointer)
  (size :int)
  (timeout :int))

(defcfun (usb-bulk-read* "usb_bulk_read") :int
  (handle device-handle-ptr)
  (endpoint :int)
  (bytes :pointer)
  (size :int)
  (timeout :int))

(defcfun (usb-interrupt-write* "usb_interrupt_write") :int
  (handle device-handle-ptr)
  (endpoint :int)
  (bytes :pointer)
  (size :int)
  (timeout :int))

(defcfun (usb-interrupt-read* "usb_interrupt_read") :int
  (handle device-handle-ptr)
  (endpoint :int)
  (bytes :pointer)
  (size :int)
  (timeout :int))

;;;; Somewhat cleaned up interface

;;; Errors
(define-condition libusb-error (error)
  ((text :initarg :text))
  (:documentation "An error from the libusb library.")
  (:report
   (lambda (condition stream)
     (write-string (slot-value condition 'text)
		   stream))))

;;; Core
(defvar *libusb-initialized* nil
  "Boolean indicating if libusb has been initialized.")

(defun usb-init ()
  "Initialize the libusb library. It's not necessary to call this
  directly, since other (Lisp) functions will do so if required."
  (unless *libusb-initialized*
    (usb-init*)
    (setf *libusb-initialized* t))
  (values))

(defun ensure-libusb-initialized ()
  "Make sure the libusb library is initialised and all busses and
  devices are found."
  (unless *libusb-initialized*
    (usb-init))
  (usb-find-busses*)
  (usb-find-devices*)
  (values))

(defun usb-find-busses ()
  "Find all of the busses on the system. Returns the number of changes,
  which specifies the total of new busses and busses removed since
  previous call to this function."
  (ensure-libusb-initialized)
  (usb-find-busses*))

(defun usb-find-devices ()
  "Find all of the devices on each bus. This should be called after
  usb-find-busses. Returns the number of changes, which specifies the
  total of new devices and devices removed since the previous call to
  this function."
  (ensure-libusb-initialized)
  (usb-find-devices*))

(defun usb-get-busses ()
  "Return a list of busses."
  (ensure-libusb-initialized)
  (loop with bus = (usb-get-busses*)
       
     until (null-pointer-p bus)
     collect bus

     do (setf bus (foreign-slot-value bus 'bus 'next))))

(defun usb-get-devices* (bus)
  "Returns a list of all devices in the given bus."
  (ensure-libusb-initialized)
  (loop with device = (foreign-slot-value bus 'bus 'devices)

     until (null-pointer-p device)
     collect device

     do (setf device (foreign-slot-value device 'device 'next))))

(defun usb-get-devices (&optional (bus-or-list (usb-get-busses)))
  "Returns a list of all usb devices. Optionally, a bus or list of
  busses can also be specified, to confine the results to devices on
  those busses."
  (if (listp bus-or-list)
      (loop for bus in bus-or-list
	 nconcing (usb-get-devices* bus))
      (usb-get-devices* bus-or-list)))

(defun usb-get-devices-by-ids (vendor-id product-id)
  "Returns a list of all devices with the given vendor id and product
  id. If any of the arguments is NIL, then the device id can match any
  value. Thus (usb-get-devices-by-ids nil nil) is equivalent
  to (usb-get-devices)."
  (flet ((ids-match (device)
	   (and (or (null vendor-id)
		    (= vendor-id (usb-get-vendor-id device)))
		(or (null product-id)
		    (= product-id (usb-get-product-id device))))))
    (delete-if-not #'ids-match (usb-get-devices))))


;;; Device operations
(defun usb-device-get-descriptor (device)
  "Returns the device descriptor for the given device."
  (foreign-slot-pointer device 'device 'descriptor))

(defun usb-get-configurations (device)
  "Returns a list of usb configurations for the given device."
  (let* ((descriptor (usb-device-get-descriptor device))
	 (total-configurations
	  (foreign-slot-value descriptor 'device-descriptor
			      'number-of-configurations)))
    (loop for index from 0 below total-configurations
       collect (inc-pointer (foreign-slot-value device
						'device
						'configuration)
			    index))))

(defun usb-configuration-get-value (configuration)
  "Returns the configuration value of the given configuration."
  (foreign-slot-value configuration 'configuration
		      'configuration-value))

(defun usb-get-configuration-by-value (device value)
  "Returns a configuration which has the given configuration value."
  (find value (usb-get-configurations device)
	:test #'(lambda (val config)
		  (= val (usb-configuration-get-value config)))))

(defun usb-configuration-get-interfaces (configuration)
  "Returns all the interfaces from the given configuration."
  (with-foreign-slots ((number-of-interfaces interface)
		       configuration configuration)
    (loop for index from 0 below number-of-interfaces
       collect (inc-pointer interface index))))

(defun usb-interface-get-settings (interface)
  "Returns all the possible settings from a given interface."
  (with-foreign-slots ((number-of-settings setting)
		       interface interface)
    (loop for index from 0 below number-of-settings
       collect (inc-pointer setting index))))

(defun usb-interface-setting-get-number (setting)
  "Return the interface number for the given interface setting."
  (foreign-slot-value setting 'setting 'interface-number))

(defun usb-interface-setting-get-alternate (setting)
  "Return the alternate interface setting value for the given setting."
  (foreign-slot-value setting 'setting 'alternate-setting))

(defun usb-interface-setting-get-endpoints (setting)
  "Return a list of endpoints for the given interface setting."
  (with-foreign-slots ((number-of-endpoints endpoint-descriptor)
		       setting setting)
    (loop for index from 0 below number-of-endpoints
       collect (inc-pointer endpoint-descriptor index))))

(defun usb-endpoint-get-address (endpoint)
  "Returns the endpoint's address."
  (foreign-slot-value endpoint 'endpoint-descriptor 'address))

(defun usb-endpoint-type (endpoint)
  "Returns the endpoint's type. This can be
  :control, :isosynchronous, :bulk or :interrupt."
  (case
      (logand (foreign-slot-value endpoint 'endpoint-descriptor 'attributes)
	      #x03)
    (0 :control)
    (1 :isosynchronous)
    (2 :bulk)
    (3 :interrupt)))

(defun usb-clear-halt (handle endpoint)
  "Clear the halt status on the specified endpoint. The endpoint can
  also be specified by its address."
  (unless (integerp endpoint)
    (setf endpoint (usb-endpoint-get-address endpoint)))
  (unless (zerop (usb-clear-halt* handle endpoint))
    (error 'libusb-error
	   :text (format nil "Error clearing halt status on endpoint with address 0x~X."
			 endpoint))))

(defun usb-reset (handle)
  "Resets the specified device by sending a RESET down the port it is
  connected to.  Note that this causes re-enumeration: After calling
  usb-reset, the device will need to re-enumerate and thusly, requires
  you to find the new device and open a new handle. The handle used to
  call usb-reset will no longer work."
  (unless (zerop (usb-reset* handle))
    (error 'libusb-error :text "Error resetting device.")))

(defun usb-claim-interface (handle setting-or-number)
  "Claim the given interface for the handle. The interface can be
  specified by its setting, or its (integer) number."
  (usb-claim-interface*
   handle
   (if (pointerp setting-or-number)
       (usb-setting-interface-get-number setting-or-number)
       setting-or-number)))

(defun usb-release-interface (handle setting-or-number)
  "Release the given interface for the handle. The interface can be
  specified by its setting, or its (integer) number."
  (usb-release-interface*
   handle
   (if (pointerp setting-or-number)
       (usb-interface-setting-get-number setting-or-number)
       setting-or-number)))

(defun usb-set-configuration (handle configuration-or-number)
  "Set the given configuration for the handle. The configuration can
  be specified also by its (integer) value."
  (usb-set-configuration*
   handle
   (if (pointerp configuration-or-number)
       (usb-configuration-get-value configuration-or-number)
       configuration-or-number)))

(defun usb-set-altinterface (handle setting-or-number)
  "Set the alternate interface setting to that of the given
  setting. The alternate interface setting can be specified by
  setting, or by its (integer) value."
  (usb-set-altinterface*
   handle
   (if (pointerp setting-or-number)
       (usb-interface-setting-get-alternate setting-or-number)
       setting-or-number)))

(defun usb-get-vendor-id (device)
  "Returns the vendor id of the device."
  (foreign-slot-value (usb-device-get-descriptor device) 'device-descriptor 'id-vendor))

(defun usb-get-product-id (device)
  "Returns the product id of the device."
  (foreign-slot-value (usb-device-get-descriptor device) 'device-descriptor 'id-product))

;;; Control transfers
(defun usb-get-string-index (device string-symbol)
  "Returns the string index associated with the given symbol. This
  symbol can be :MANUFACTURER, :PRODUCT or :SERIAL-NUMBER."
  (let ((descriptor (usb-device-get-descriptor device)))
    (foreign-slot-value descriptor
			'device-descriptor
			(intern (concatenate 'string "INDEX-" (string string-symbol))
				:libusb-ffi))))

(defun usb-get-string (device-handle index &optional language-id)
  "Returns the string descriptor specified by index and langid from a
  device. The string will be returned in Unicode as specified by the
  USB specification. If language id is nil (the default), returns the
  string descriptor specified by index in the first language for the
  descriptor and converts it into C style ASCII. Returns the number of
  bytes returned."
  (let (bytes-read string)
    (setf string
	  (if language-id
	      (with-foreign-pointer-as-string ((buffer buffer-size) 128 :encoding :utf-16)
		(setf bytes-read
		      (usb-get-string* device-handle index language-id buffer buffer-size)))
	      (with-foreign-pointer-as-string ((buffer buffer-size) 128 :encoding :ascii)
		(setf bytes-read
		      (usb-get-string-simple* device-handle index buffer buffer-size)))))
    (if (< bytes-read 0)
	(error 'libusb-error :text (format nil "Error reading string at index ~D~@[ with language id ~D~]."
					   index language-id))
	string)))


;;; Bulk transfers
(defun usb-bulk-write (handle endpoint buffer timeout)
  "Perform a bulk write request to the endpoint, which can
  alternatively be specified by its address. Buffer should be a
  foreign array of type vector-unsigned-byte-8. Returns number of
  bytes written."
  (unless (integerp endpoint)
    (setf endpoint (usb-endpoint-get-address endpoint)))
  (let* ((bytes-to-write (grid:dim0 buffer))
	 (bytes-written
	  (usb-bulk-write* handle endpoint
			   (grid::foreign-pointer buffer)
			   (grid:dim0 buffer) timeout)))
    (if (< bytes-written 0)
	(error 'libusb-error :text "Bulk write failed.")
	bytes-written)))
      
(defun usb-bulk-read (handle endpoint bytes-to-read timeout)
  "Perform a bulk read request to the endpoint, which can be specified
  by its address or pointer to the endpoint. Returns the buffer of
  bytes read, which is of type vector-unsigned-byte-8."
  (unless (integerp endpoint)
    (setf endpoint (usb-endpoint-get-address endpoint)))
  (let* ((buffer (grid:make-foreign-array
		  '(unsigned-byte 8)
		  :dimensions bytes-to-read))
	 (bytes-read
	  (usb-bulk-read* handle endpoint
			  (grid::foreign-pointer buffer)
			  bytes-to-read timeout)))
    (if (< bytes-read 0)
	(error 'libusb-error :text "Bulk read failed.")
	(slice buffer `(:range 0 ,(- bytes-read 1))))))

;;; Interrupt transfers
(defun usb-interrupt-write (handle endpoint buffer timeout)
  "Perform an interrupt write request to the endpoint, which can
  alternatively be specified by its address. Buffer should be a
  foreign array of type vector-unsigned-byte-8. Returns number of
  bytes written."
  (unless (integerp endpoint)
    (setf endpoint (usb-endpoint-get-address endpoint)))
  (let* ((bytes-to-write (grid:dim0 buffer))
	 (bytes-written
	  (usb-interrupt-write* handle endpoint
				(grid::foreign-pointer buffer)
				bytes-to-write timeout)))
    (if (< bytes-written 0)
      (error 'libusb-error :text "Interrupt write failed.")
      bytes-written)))

(defun usb-interrupt-read (handle endpoint bytes-to-read timeout)
  "Perform an interrupt read request to the endpoint, which can be
  specified by its address or pointer to the endpoint. Returns the
  buffer of bytes read, which is of type vector-unsigned-byte-8."
  (unless (integerp endpoint)
    (setf endpoint (usb-endpoint-get-address endpoint)))
  (let* ((buffer (grid:make-foreign-array
		 '(unsigned-byte 8)
		 :dimensions bytes-to-read))
	 (bytes-read
	  (usb-interrupt-read* handle endpoint
			       (grid::foreign-pointer buffer)
			       bytes-to-read timeout)))
    (if (< bytes-read 0)
	(error 'libusb-error :text "Interrupt read failed.")
	(slice buffer `(:range 0 ,(- bytes-read 1))))))

(defun endpoint-in-p (endpoint)
  "Check if an endpoint is an in endpoint (and thus can be read from)."
  (unless (integerp endpoint)
    (setf endpoint (usb-endpoint-get-address endpoint)))
  (= (logand #x80 endpoint) #x80))

(defun endpoint-out-p (endpoint)
  "Check if an endpoint is and out endpoint (and thus can be written to)."
  (not (endpoint-in-p endpoint)))
