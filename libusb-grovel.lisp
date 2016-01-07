;;;; libusb-grovel.lisp

(in-package :libusb-ffi)

#+windows
(cc-flags "-ID:/home/soemraws/downloads/libusb-win32-bin-1.2.2.0/include"
	  "-LD:/home/soemraws/downloads/libusb-win32-bin-1.2.2.0/lib/gcc")

(include "usb.h")

(constantenum class
	      ((:per-interface "USB_CLASS_PER_INTERFACE"))
	      ((:audio "USB_CLASS_AUDIO"))
	      ((:comm "USB_CLASS_COMM"))
	      ((:hid "USB_CLASS_HID"))
	      ((:printer "USB_CLASS_PRINTER"))
	      ((:ptp "USB_CLASS_PTP"))
	      ((:mass-storage "USB_CLASS_MASS_STORAGE"))
	      ((:hub "USB_CLASS_HUB"))
	      ((:data "USB_CLASS_DATA")))

(constantenum descriptor-type
	      ((:device "USB_DT_DEVICE"))
	      ((:config "USB_DT_CONFIG"))
	      ((:string "USB_DT_STRING"))
	      ((:interface "USB_DT_INTERFACE"))
	      ((:endpoint "USB_DT_ENDPOINT"))
	      ((:hid "USB_DT_HID"))
	      ((:report "USB_DT_REPORT"))
	      ((:physical "USB_DT_PHYSICAL"))
	      ((:hub "USB_DT_HUB")))

(constantenum descriptor-type-size
	      ((:device "USB_DT_DEVICE_SIZE"))
	      ((:config "USB_DT_CONFIG_SIZE"))
	      ((:interface "USB_DT_INTERFACE_SIZE"))
	      ((:endpoint "USB_DT_ENDPOINT_SIZE"))
	      ((:endpoint-audio "USB_DT_ENDPOINT_AUDIO_SIZE"))
	      ((:hub-nonvar "USB_DT_HUB_NONVAR_SIZE")))

(constantenum request
              ((:get-status "USB_REQ_GET_STATUS"))
              ((:clear-feature "USB_REQ_CLEAR_FEATURE"))
              ((:set-feature "USB_REQ_SET_FEATURE"))
              ((:set-address "USB_REQ_SET_ADDRESS"))
              ((:get-descriptor "USB_REQ_GET_DESCRIPTOR"))
              ((:set-descriptor "USB_REQ_SET_DESCRIPTOR"))
              ((:get-configuration "USB_REQ_GET_CONFIGURATION"))
              ((:set-configuration "USB_REQ_SET_CONFIGURATION"))
              ((:get-interface "USB_REQ_GET_INTERFACE"))
              ((:set-interface "USB_REQ_SET_INTERFACE"))
              ((:synch-frame "USB_REQ_SYNCH_FRAME")))
              
(constantenum type
              ((:standard "USB_TYPE_STANDARD"))
              ((:class "USB_TYPE_CLASS"))
              ((:vendor "USB_TYPE_VENDOR"))
              ((:reserved "USB_TYPE_RESERVED")))

(constantenum recip
              ((:device "USB_RECIP_DEVICE"))
              ((:interface "USB_RECIP_INTERFACE"))
              ((:endpoint "USB_RECIP_ENDPOINT"))
              ((:other "USB_RECIP_OTHER")))

(constantenum endpoint
              ((:in "USB_ENDPOINT_IN"))
              ((:out "USB_ENDPOINT_OUT")))

(cstruct bus "struct usb_bus"
	 (next "next" :type :pointer)
	 (previous "prev" :type :pointer)
	 (devices "devices" :type :pointer))

(cstruct endpoint-descriptor "struct usb_endpoint_descriptor"
	 (address "bEndpointAddress" :type :uint8)
	 (sync-address "bSynchAddress" :type :uint8)
	 (attributes "bmAttributes" :type :uint8))

(cstruct setting "struct usb_interface_descriptor"
	 (interface-number "bInterfaceNumber" :type :uint8)
	 (alternate-setting "bAlternateSetting" :type :uint8)
	 (number-of-endpoints "bNumEndpoints" :type :uint8)
	 (endpoint-descriptor "endpoint" :type :pointer))

(cstruct interface "struct usb_interface"
	 (setting "altsetting" :type :pointer)
	 (number-of-settings "num_altsetting" :type :int))

(cstruct configuration "struct usb_config_descriptor"
	 (number-of-interfaces "bNumInterfaces" :type :uint8)
	 (configuration-value "bConfigurationValue" :type :uint8)
	 (interface "interface" :type :pointer))

(cstruct device-descriptor "struct usb_device_descriptor"
	 (device-class "bDeviceClass" :type class)
	 (id-vendor "idVendor" :type :uint16)
	 (id-product "idProduct" :type :uint16)
	 (index-manufacturer "iManufacturer" :type :uint8)
	 (index-product "iProduct" :type :uint8)
	 (index-serial-number "iSerialNumber" :type :uint8)
	 (number-of-configurations "bNumConfigurations" :type :uint8))

(cstruct device "struct usb_device"
	 (next "next" :type :pointer)
	 (previous "prev" :type :pointer)
	 (descriptor "descriptor" :type (:pointer (:struct device-descriptor)))
	 (configuration "config" :type :pointer)
	 (number-of-children "num_children" :type :uint8)
	 (children "children" :type :pointer))

(ctype size-t "size_t")
