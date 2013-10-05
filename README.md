# Hexwax ExpandIO USB Application

This is some Haskell code that requires the use of "libusb". You should first build and install it on your platform. I can ensure you this works on Linux and OS X (see later) but I have never tried it on Windows as I don't use it.

Having built and installed libusb, install the packages `bindings-libusb` and `usb` and you are good to go with your Haskell installation.


# The Code…

…is proof of concept and probably not as neat as it could be but hell, it works and that's all I wanted or needed it to do. Currently it has application code to control a coil-winder and a temperature controlled hand-crocheted Owl shaped tea cosy. Because I can.



## OSX -- Nightmare!

When I first tried to get this to run under OS X, I had to learn the hard way that the second you plug the device into the machine the kernel jumps in and steals it and assigns it a default handler. This initially caused me problems as the OS X version of "libusb" doesn't actually unbind kernel handlers when asked to do so. To cut a long and painful story short I sorted it with some help from the Apple USB mailing list. Read on...


# Granting user permissions using "udev"

When you plug the expandIO-USB device into your Linux machine, it
should appear in the list of USB devices if you use the "lsusb"
command like this:


    Bus 002 Device 002: ID 046d:c31c Logitech, Inc. Keyboard K120 for Business
    Bus 003 Device 002: ID 046d:c050 Logitech, Inc. RX 250 Optical Mouse
    Bus 004 Device 002: ID 0a5c:4500 Broadcom Corp. BCM2046B1 USB 2.0 Hub (part of BCM2046 Bluetooth)
    Bus 005 Device 002: ID 0b40:0132
    Bus 001 Device 001: ID 1d6b:0002 Linux Foundation 2.0 root hub
    Bus 002 Device 001: ID 1d6b:0001 Linux Foundation 1.1 root hub
    Bus 003 Device 001: ID 1d6b:0001 Linux Foundation 1.1 root hub
    Bus 004 Device 001: ID 1d6b:0001 Linux Foundation 1.1 root hub
    Bus 005 Device 001: ID 1d6b:0001 Linux Foundation 1.1 root hub
    Bus 004 Device 003: ID 0a5c:4502 Broadcom Corp. Keyboard (Boot Interface Subclass)
    Bus 004 Device 004: ID 0a5c:4503 Broadcom Corp. Mouse (Boot Interface Subclass)
    Bus 004 Device 005: ID 050d:016a Belkin Components Bluetooth Mini Dongle


However, that doesn't mean that the device is accessible, indeed at
this point it would require you to be "root" in order to be able to
send commands to it; this is because it hasn't yet been configured
properly. It's simple if you follow these steps but please bear in
mind that the Linux distro used was "Mint/14", YMMV.


## Identifying the device

The simplest way to get the information that we need is to open a
terminal window and then issue this command:

    tail -f /var/log/syslog


Then plug in your development board / rig / whatever and then watch
the output from "tail", you should see something that looks almost
identical to this, the product and vendor identification numbers
should be "0xb30:0132" whatever Linux distro you use as that is how
the PIC device is programmed at the factory! Here is the output:

    kernel: [ 1961.104065] usb 5-1: >USB disconnect, device number 3
    kernel: [ 1964.520027] usb 5-1: >new full-speed USB device number 4 using uhci_hcd
    kernel: [ 1964.764051] usb 5-1: >New USB device found, idVendor=0b40, idProduct=0132
    kernel: [ 1964.764058] usb 5-1: >New USB device strings: Mfr=2, Product=1, SerialNumber=3
    kernel: [ 1964.764063] usb 5-1: >Product: expandIO-USB
    kernel: [ 1964.764067] usb 5-1: >Manufacturer: Firmware Factory Ltd
    kernel: [ 1964.764070] usb 5-1: >SerialNumber: 21436587-2455-011F080C7-02AF8598B518
    mtp-probe: checking bus 5, device 4: "/sys/devices/pci0000:00/0000:00:1d.3/usb5/5-1"
    mtp-probe: bus: 5, device: 4 was not an MTP device
    kernel: [ 1964.777332] hid-generic 0003:0B40:0132.0008: >hiddev0,hidraw5: USB HID v1.11 Device [Firmware Factory Ltd expandIO-USB] on usb-0000


## Getting the path to the USB device

So far so good. The piece of information that we want is the path to
the device as seen by the system, in this case it is:

    /sys/devices/pci0000:00/0000:00:1d.3/usb5/5-1


## Getting the key names and device identifiers

Using that piece of information we can now use the "udevadm" program
to dump everything it knows about our device:

    udevadm info -a -p /sys/devices/pci0000:00/0000:00:1d.3/usb5/5-1

This will produce a monster load of output! Don't be put off, we
nearly have what we want to be able to write a custom "udev" rule so
that we don't have to run as root all the time. Here it is, but
truncated so that we don't get swamped with too much info:

  looking at device '/devices/pci0000:00/0000:00:1d.3/usb5/5-1':
    KERNEL=="5-1"
    SUBSYSTEM=="usb"
    DRIVER=="usb"
    ATTR{idVendor}=="0b40"
    ATTR{serial}=="21436587-2455-011F080C7-02AF8598B518"
    ATTR{version}==" 2.00"
    ATTR{urbnum}=="20"
    ATTR{manufacturer}=="Firmware Factory Ltd"
    ATTR{removable}=="unknown"
    ATTR{idProduct}=="0132"
    ATTR{bDeviceClass}=="00"
    ATTR{product}=="expandIO-USB"


What we need is JUST the "idVendor" and "idProduct" values *which we
already knew* but the reason for the above step is to get the system
to tell us how it names the keys, it has been known to vary between
"udev" versions so getting the "udevadm" to tell us is getting it
straight from the source, we then only have to use the same key names
in our new rule file to guarantee success. Here is the filename you
must create as root:

    /etc/udev/rules.d/hexwax-expandio.rules

Here is what should go into it:

    # Hexwax expandIO-USB chip, unmodified product and vendor id assumed.
    SUBSYSTEM=="usb", ACTION=="add", ATTR{idVendor}=="0b40",
        ATTR{idProduct}=="0132", GROUP="users", MODE="0666"

(Broken into two lines for clarity but please make sure that you enter
it as *a single line* and ensure that you have used '==' not '=' if
you manually type it).

What this says to the system is that the next time you plug in your
device, it is to make it accessibhle to the group "users" (that's you)
and that it will have full read and write permissions. Once you have
saved this file, "udev" will notice the change to the folder and
automatically take the new rule and process it.

If you have followed all of the above steps correctly then you should
be able to develop programs for your device and not be root.


# OS X and HID Devices

When you plug your device into OS X, the kernel will grab it. That's
not good and means for example that you won't be able to use "libusb"
to write code to play with it. In order to prevent that from
happening, you will need to install the supplied "codeless kext" by
following these steps, assuming that you are in the terminal window
and currently in the same folder as this README file:

    $ sudo cp -r HexwaxShield.kext /System/Library/Extensions/.
    $ sudo chown -R root:wheel /System/Library/Extensions/HexwaxShield.kext
    $ sudo kextload -vt /System/Library/Extensions/HexwaxShield.kext/
    Notice: -print-diagnostics (-t) ignored; use kextutil(8) to test kexts.
    Requesting load of /System/Library/Extensions/HexwaxShield.kext.
    /System/Library/Extensions/HexwaxShield.kext loaded successfully (or already loaded).

Now when you plug the device in, the kernel will no longer
automatically grab it and wrap it, instead it will be left alone which
means that your code can talk to it instead. If you don't install the
extension then you will have to use IOKit to manage it. If that's what
you want then that's fine if you developing a product specifically for
the Mac market but if you want to maintain cross-platform code using
libusb then you will need to do the above.

