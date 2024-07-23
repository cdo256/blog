+++
title = "Systemd on Guix System - How hard could it be?"
author = ["Christina O'Donnell"]
description = "A preliminary investigation"
date = 2023-12-30
keywords = ["Systemd", "Guix", "Guix System", "Linux", "open source"]
linkTitle = "Systemd on Guix System - How hard could it be?"
draft = false
[menu]
    [menu.main]
        parent = "blog"
+++

> To be honest, while the Shepherd has been a fun hack, I’ve been more and
> more feeling that yeah, it wouldn’t cut it in the long term (it’s also
> become clearer Scheme as convenient as C when it comes to systems
> programming and things like dealing with dependency graphs.)
>
> <footer>
>
> -- Ludovic Courtès, 2018
> (quoted **grossly** out of [context](https://lists.gnu.org/archive/html/guix-devel/2018-04/msg00002.html))
>
> </footer>


## Introduction {#introduction}

This post explores the concept of integrating Systemd (a widely used init
system in the Linux ecosystem) as the init system for GNU/Guix, a
transactional package manager and Linux distribution. We trace the history of
this idea, its reception, and outline how I think it could be achieved.

The current init system of Guix is GNU/Shepherd. This is a service manager
written in Guile (do I need to keep adding the 'GNU/' prefix?), which is the
same language as Guix.


## Okay, but why? {#okay-but-why}

****Familiarity and Compatibility****: Many users transitioning to GNU Guix may be
accustomed to systemd, which is the standard in numerous Linux distributions.
They might have a preference for systemd or possess existing systemd unit files
they wish to continue using. This familiarity can be a significant factor in
their choice of init system.

****Performance****: Systemd is notable for its rapid parallelization
of start-up processes, potentially resulting in quicker boot times. It offers an
array of features like socket activation, service monitoring, and cgroups that
users (such as myself) might find advantageous.

****Integration with Software****: Certain applications are specifically designed to
leverage systemd's capabilities in service management and logging. Users
dependent on such software might naturally gravitate towards systemd for
compatibility reasons.

****Community and Documentation****: Systemd's widespread use has led to the formation
of a robust community and the development of comprehensive documentation. This
extensive support can be invaluable for troubleshooting and educational
purposes.

****System Management****: Systemd encompasses a broad array of system management tools,
including journald for logging and udev for device initialization. This suite
can simplify various system administration tasks.


## History of Systemd in the context of Guix {#history-of-systemd-in-the-context-of-guix}

-   In 2018, a playful April Fools' patch was sent to the Guix development list,
    proposing the addition of a Systemd package. This was initially a joke, but
    it sparked a conversation about the serious integration of Systemd into
    Guix.
    -   Source: [April Fools' Systemd Patch](https://lists.gnu.org/archive/html/guix-devel/2018-04/msg00001.html)
-   A Google Summer of Code candidate was accepted to improve Shepherd, which
    would have allowed it to accept unit files and manage cgroups.
-   In 2021, an extended version of the 2018 patch was submitted to the Guix
    project. However, it faced rejection due to compatibility issues and a lack
    of significant benefits for free software packages.
    -   Discussion Link: [Add systemd - Guix](https://issues.guix.gnu.org/48924)


## Implementation {#implementation}

Here I'm just considering Guix System services, though I expect Guix Home
services would follow a similar philosophy.


### Directories {#directories}

Here I'm letting the variable `$GUIX_SYSTEM_PROFILE` correspond to the profile
of the current running system. This is typically
`/var/guix/profiles/system/profile/`.

-   `$GUIX_SYSTEM_PROFILE/lib/systemd/system` - This directory will contain the
    package service and other files.
-   `/etc/systemd/system` - This can be used for enabling and disabling services
    at each target. There are two options that I can think of for this directory:
    -   One is to populate it with store items. This would allow `guix system
             reconfigure` to set up a running system without requiring the user to
        manually populate `/etc`. But it would also mean that `systemctl` wouldn't
        be able to function, as it works on /etc files.
    -   Another option is to have any file in `/etc` shadow its corresponding
        profile item. This would have to be set up such that the existence of
        `/etc/xyz.target.wants` overrides the corresponding the wants in
        `$GUIX_SYSTEM_PROFILE/etc/xyz.target.wants`. That way a unit could be
        removed from wants by the user. Otherwise they'd always have the default
        wants enabled for each target.
-   `/run/systemd/system` - This directory could be used to manage runtime
    services, with no changes envisioned.


### Strategy {#strategy}

Each unit will be implemented as a G-expressions on a Scheme
`<systemd-service>` record. When these are lowered, they produce a
file-like object containing a distinct systemd unit file, which is placed
in `$GUIX_SYSTEM_PROFILE/lib/systemd/system`.

Systemd will be configured to look in these directories and interpret
relative paths appropriately.

What could end up being a bit of headache is that disabling services won't be
able to be done with `systemctl`, and would have to be done with `guix system
reconfigure`.


## NixOS uses Systemd. Why don't I just use NixOS? {#nixos-uses-systemd-dot-why-don-t-i-just-use-nixos}

I have to admit, I do feel a bit silly suggesting adding Systemd to Guix. NixOS,
the package that Guix is based on, comes with Systemd as the sole init system.
Why don't I just go and use that instead? Why do I have to corrupt the beautiful
Scheme operating system with mainstream programs written in mainstream
languages?

This is a good question and I don't feel like I've got a well defensible answer.
I like Scheme and prefer it to Nix's bespoke language. I don't see any reason
why a Scheme daemon needs to be be ran with a PID 1 interpreter.


## Conclusion {#conclusion}

If you think that I'm wrong, and have a clear reason why, please send me an
email at [cdo@mutix.org](mailto:cdo@mutix.org). I really want to know so I don't trudge through code
trying to hack this darn thing together only to realize that the Shepherd guile
is inseparably tangled in the threads of Guix's scheme.

_Disclaimer: I hope my intended light-hearted humor came across here. I don't
mean to incite any init-system war. Shepherd is a fantastic peice of
engineering, I just personally miss the feeling of using Systemd, and it would
be empowering for users if they could choose their own init systems._
