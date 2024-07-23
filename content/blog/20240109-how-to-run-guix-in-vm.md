+++
title = "How to Share /gnu/store between a Host and a Guest Guix System"
author = ["Christina O'Donnell"]
date = 2024-01-09
draft = false
[menu]
    [menu.main]
        parent = "blog"
+++

## Introduction {#introduction}

If you've ever tried out [`guix system vm`](https://guix.gnu.org/manual/en/guix.html#Invoking-guix-system), you've probably found that running
[Guix](https://guix.gnu.org/manual/en/html_node/Invoking-guix_002ddaemon.html) commands in the guest system simply don't work.

```text
root@test-vm# guix build hello
guix build: error: opening lock file /gnu/store/...-mirrors.lock: Read-only file system
```

These do, however, work using `guix system image`, which is a much heavier,
fully independent VM image. However, these are a lot slower, requiring every
package to be built from scratch. Doing any Guix command requires downloading
and rebuilding many _many_ packages that you already have on your host machine.

One approach to avoid this could be to reuse your existing Guix daemon process.
This way, you only build what you don't already have in your main system, saving
a _lot_ of time.

This post explains how you can share `/gnu/store/` with a guest Guix System VM
instance. The approach was [suggested by Ludovic Courtès](https://issues.guix.gnu.org/39815) in 2020. It assumes that
you are running Guix System, but can be followed if you have Guix installed on
any machine.

This method works by connecting the guest Guix commands to the hosts Guix daemon
through TCP. This is possible through making just two changes:

1.  Allow Guix daemon to accept connections through TCP using `--listen`.
2.  Set the `GUIX_DAEMON_SOCKET` environment variable inside the guest to
    connect to this socket.


## Make `guix-daemon` listen over TCP {#make-guix-daemon-listen-over-tcp}

The first step is to change your Guix daemon to accept connections over TCP. To
do this you need to pass in the `--listen` parameter to Guix daemon.

To add parameters to Guix daemon on your host Guix System, you need to edit your
services to add the `--listen` parameter to the guix-service-type, like so:

```Scheme
(operating-system
  ...
  (services
    (cons* ...
           (modify-services %desktop-services
             ...
             (guix-service-type config =>
                                (guix-configuration
                                  (inherit config)
                                  (extra-options '("--listen=localhost:44146"
                                                   "--listen=/var/guix/daemon-socket/socket")))))))
```

Port 44146 is the default for Guix protocol, and
`/var/guix/daemon-socket/socket` is the default UNIX socket location.

Here we're using [`modify-services`](https://guix.gnu.org/manual/en/html_node/Service-Reference.html), which goes through all of the %desktop
services and makes a specified change. If you don't have a modify-services
already, then you'll need to create one if you're current services depends on
`%desktop-services` or `%base-services` or similar, having the modify-services
expression go in the place `%desktop-services` used to go.

If you already have a `guix-service-type` rule, then you'd want to simply add
the `(extra-options ...)` to that existing rule. Otherwise, you can use the
format above as-is.

For reference, here's links to where you can find the documentation for:
[Guix System](https://guix.gnu.org/manual/en/html_node/System-Configuration.html), [`modify-services`](https://guix.gnu.org/manual/en/html_node/Service-Reference.html), [`guix-service-type`](https://guix.gnu.org/manual/en/html_node/Base-Services.html), and [`--listen`](https://guix.gnu.org/manual/en/html_node/Invoking-guix_002ddaemon.html).

If you're running Guix on a foreign distro, then you can edit the init script.
As an example, if you run a 'normal' distro that runs [Systemd](https://systemd.io/), then you'd add
the `--listen` flags to the [`ExecStart`](https://www.freedesktop.org/software/systemd/man/latest/systemd.service.html) line, like so:

```Systemd
# /etc/systemd/system/guix-daemon.service
# This is a "service unit file" for the systemd init system to launch
# 'guix-daemon'.  Drop it in /etc/systemd/system or similar to have
# 'guix-daemon' automatically started.

[Unit]
Description=Build daemon for GNU Guix

[Service]
ExecStart=/usr/bin/guix-daemon --build-users-group=_guixbuild --listen=localhost:44146 --listen=/var/guix/daemon-socket/socket
Environment='GUIX_LOCPATH=/var/guix/profiles/per-user/root/guix-profile/lib/locale' LC_ALL=en_GB.utf8
RemainAfterExit=yes

# See <https://lists.gnu.org/archive/html/guix-devel/2016-04/msg00608.html>.
# Some package builds (for example, go@1.8.1) may require even more than
# 1024 tasks.
TasksMax=8192

[Install]
WantedBy=multi-user.target
```


## Declaring the guest system {#declaring-the-guest-system}

Next we want a Guix System declaration that we can use to boot up a VM with.
This guide should work regardless of what you use for this. For this tutorial
we'll use a rather minimal VM definition, useful for singling out errors.

Save the following as `test-vm.scm`. Include any packages or services that you desire.

```Scheme
;; ~/test-vm.src
(use-modules (gnu))
(use-service-modules networking)
(use-package-modules bootloaders)

(define %keyboard-layout
  (keyboard-layout "gb"))

(operating-system
  (locale "en_GB.utf8")
  (timezone "Etc/UTC")
  (keyboard-layout %keyboard-layout)
  (host-name "test-vm")
  (packages (cons* (specification->package "net-tools")
                   %base-packages))
  (services
  (cons* (service dhcp-client-service-type)
          %base-services))
  (bootloader
  (bootloader-configuration
    (bootloader grub-bootloader)
    (targets '("/dev/vda"))
    (terminal-outputs '(console))
    (keyboard-layout %keyboard-layout)))
  (file-systems
  (cons* (file-system
          (mount-point "/")
          (device "/dev/vda1")
          (type "ext4"))
          %base-file-systems)))
```

Now it's time to build the VM. The following builds the VM image and starts it
up in one command.

```Shell
sudo $(guix system vm test-vm.scm) -nic user,model=virtio-net-pci --enable-kvm
```

This runs a script that wraps a call to `qemu-*` so the options after the `$()`
are interpreted by QEMU (typically `qemu-x86_64`), so you can consult [their
documentation](https://www.qemu.org/docs/master/system/qemu-manpage.html) for more.[^fn:1]


## Connecting to the Guix daemon from inside the guest {#connecting-to-the-guix-daemon-from-inside-the-guest}

If that succeeds then we're inside the VM. You can login as root without a
password. Here we can use the default host gateway IP, which is typically
10.0.2.2. This address from the guest corresponds to the host's local IP. So
writing the following ensure Guix connects to your machine's Guix daemon.

```text
# export GUIX_DAEMON_SOCKET=guix://10.0.2.2:44146
```

If this works then you should be able to run any Guix command successfully. As
mentioned before, 44146 is the default port of the Guix protocol.

```text
# guix build hello
/gnu/store/6fbh8phmp3izay6c0dpggpxhcjn4xlm5-hello-2.12.1
```


## Conclusion {#conclusion}

This setup streamlines development and testing workflows, making it a valuable
tool for Guix users. Stay updated with Guix's evolving ecosystem to make the
most of this powerful package manager.

In this guide, you've learned how to efficiently share the `/gnu/store`
directory between a host and guest Guix System, saving time and compute. By
configuring the Guix daemon to listen over TCP, you can seamlessly steal
packages from the host without anyone noticing they're missing!

If you encounter issues or have suggestions for improvements, please reach out
at [cdo@mutix.org](mailto:cdo@mutix.org).

Happy hacking!

[^fn:1]: If you get an error like `/dev/kvm is not found`, then it means that
    virtualization isn't enabled in hardware. If you get an error saying that
    `qemu-kvm is missing`, then it's to do with hardware virtualization, not a
    missing package, as qemu-kvm is always installed with `qemu` on Guix. Instead it
    means that you'll need to reboot into BIOS settings and enable 'vmx' or 'svm'
    under the CPU menu. Alternatively, you can remove the `--enable-kvm` altogether.
