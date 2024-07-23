+++
title = "How to Make your own Guix Channel"
author = ["Christina O'Donnell"]
date = 2024-03-03
keywords = ["Channels", [",", "Guix"], [",", "Linux"], [",", "Open"], "Source", [",", "GNU"], "System"]
draft = false
[menu]
    [menu.main]
        parent = "blog"
+++

## Understanding Channels {#understanding-channels}

[GNU/Guix](https://guix.gnu.org/) is a [transactional package manager](https://en.wikipedia.org/wiki/GNU_Guix#Transactional_upgrades) and an advanced distribution of the
GNU system that "respects user freedom". As a powerful tool, Guix allows users to create their
own package repositories in the form of Git repositories called channels.

This allows you to host your own packages to share with the world, or just
with your friends.

Channels are a repository of Guix package definitions hosted using Git. This
functionality opens up an avenue for users to extend Guix and its package
collection, enabling the addition of custom software or services that may not be
available in the default Guix repository.

You can create public or private channels, however private channels only work
through SSH, which isn't supported by all Git hosting platforms (for example
GitHub).

You don't have to know Scheme to follow this, but some familiarity with Guix
will be assumed. We will make frequent reference to [Guix Home](https://guix.gnu.org/en/blog/2022/keeping-ones-home-tidy/). As ever, the
first place you should look for answers is in the [Guix Reference Manual](https://guix.gnu.org/manual). In
particular the section [Creating a Channel](https://guix.gnu.org/manual/en/guix.html#Creating-a-Channel) is worth a look at if you haven't
already. Beyond this, Peter Lo has written a fantastic in-depth [blog post](https://peterloleungyau.github.io/post/more_guix_private_channel/) that
covers the same topic.


### Role of Channels {#role-of-channels}

Channel repositories consist of Guile modules that export packages or services.
These packages then become accessible via any 'guix' command, once the
channel is added to the Guix channels using `guix pull`.


### What can you use a channel for? {#what-can-you-use-a-channel-for}

The use of channels provides numerous advantages:

1.  **Packages**: Channels can host a wide array of software, including those not
    available in the default Guix package collection. Channels can be pinned to
    specific commits, ensuring complete reproducibility of the software
    environment across multiple machines. Or you could get more up-to-date
    versions than are currently in Guix's main channel.
2.  **Configuration compnonents**: Services for Guix Home and Guix System can be
    included in channels. In fact Guix Home itself started life in a [third-party channel](https://git.sr.ht/~abcdw/rde).
3.  **Modding**: Is there some feature of a program you use that you find annoying
    but can't turn off? With channels, you can check out that code, disable the
    feature, and apply the patch to the package definition.


## Creating a Channel {#creating-a-channel}

Creating a channel involves several steps, including setting up a Git
repository, writing package definitions in Guile modules, and making the
channel available to Guix commands.

You will need Guix, GnuPG, and Git installed. You'll also want server to host
your channel, although local channels are possible too.

If you have decided that you'd like your channel to be private, then this is
possible, but requires a few extra steps. We'll cover both scenarios, starting
with public channels.


### Setting up GnuPG {#setting-up-gnupg}

Before we start, you need to make sure that you're set up to sign commits with
GnuPG. There's several guides out there that you can follow: [This one by Fedora](https://docs.fedoraproject.org/en-US/quick-docs/create-gpg-keys/)
is well structured and should answer most of the questions you'd have.

Once you've got GnuPG set up and have generated a keypair, you'll want to add
your signing key to Git.

<a id="org-example-block--.config-git-config"></a>
```text
[user]
	email = cdo@mutix.org
	name = Christina O'Donnell
	signingkey = 358B3178BAC1BA9B105C5CDA344C0AA8A41E474A

[commit]
	gpgsign = true
```

`gpgsign = true` tells Git to always sign commits without having to pass `-S`.
This is optional but I find it to be a useful default.


### Get Git signing key id {#get-git-signing-key-id}

Next you need to get your Git signing key. This is a different way of writing
your GnuPG. It has the same length as your GnuPG key, but will be seen by Git
when signing commits. One easy way to check this is to just sign a commit and
then verify the commit to see the key ID used.

```text
cdo@desktop ~$ gpg -K
/home/cdo/.local/secure/gnupg/pubring.kbx
-----------------------------------------
sec#  rsa3072 2022-09-01 [C]
      358B3178BAC1BA9B105C5CDA344C0AA8A41E474A
uid           [ultimate] Christina O'Donnell (master) <cdo@mutix.org>
ssb>  rsa2048 2022-09-05 [A] [expires: 2024-09-04]
ssb>  rsa2048 2022-09-05 [S] [expires: 2024-09-04]
ssb>  rsa4096 2022-09-20 [E] [expires: 2024-09-19]
ssb>  rsa2048 2022-09-21 [S] [expires: 2024-09-20]
ssb>  rsa2048 2022-09-21 [A] [expires: 2024-09-20]

cdo@desktop ~$ mkdir /tmp/test-repo; cd /tmp/test-repo
cdo@desktop /tmp/test-repo$ git init
Initialized empty Git repository in /tmp/test-repo/.git/
cdo@desktop /tmp/test-repo$ touch a
cdo@desktop /tmp/test-repo$ git add a
cdo@desktop /tmp/test-repo$ git commit -S 358B3178BAC1BA9B105C5CDA344C0AA8A41E474A -m "Test"

[master (root-commit) fd61ab6] Test
 1 file changed, 0 insertions(+), 0 deletions(-)
 create mode 100644 a
```

Here I have signed a test commit using the key 358B3178 in the `gpg -K` output.

```text
cdo@desktop /tmp/test-repo$ git verify-commit HEAD
gpg: Signature made Sun 03 Mar 2024 14:27:04 GMT
gpg:                using RSA key D899861B5EAD198ACA062A9B9DA1DD5253A7AA4C
gpg: Good signature from "Christina O'Donnell (master) <cdo@mutix.org>" [ultimate]
```

However `git verify-commit` sees a signed commit using a different key name.
It's this name (D899861B...) that we want to use.


### Setting Up a Git Repository {#setting-up-a-git-repository}

To create a channel, you first need to establish a Git repository that will
home your Scheme files. These files provide the package definitions. To make
the repository a channel, initiate it as a Git repository, add your `.scm`
files, and make a signed commit.

```text
cdo@desktop ~$ mkdir ~/src/cdo-guix-channel; cd ~/src/cdo-guix-channel
cdo@desktop ~/src/cdo-guix-channel$ git init
```

Then add `.guix_channel` and `.guix_authorizations` to the directory:

<a id="code-snippet--.guix-channel"></a>
```Scheme
(channel
  (version 0)
  (url "https://git.mutix.org/cdo-guix-channel.git"))
```

<a id="code-snippet--.guix-authorizations"></a>
```Scheme
(authorizations
  (version 0)
  (("D899 861B 5EAD 198A CA06   2A9B 9DA1 DD52 53A7 AA4C"
   (name "cdo"))))
```

Here's what my checkout had in it:

```text
drwxr-xr-x  8 cdo cdo 4096 Mar  3 17:48   .git
-rw-r--r--  1 cdo cdo  107 Mar  3 17:36   .guix-authorizations
-rw-r--r--  1 cdo cdo   77 Mar  3 17:36   .guix-channel
```

Commit these with `-S` to sign it.

```text
cdo@desktop ~/src/cdo-guix-channel$ git add .
cdo@desktop ~/src/cdo-guix-channel$ git commit -S -m "Initial commit"
```

Next, create a `keyring` branch based off `master`. And add your public key to
the repo like so:

```text
cdo@desktop ~/src/cdo-guix-channel$ git branch keyring master
cdo@desktop ~/src/cdo-guix-channel$ gpg -a --export cdo@mutix.org >cdo-358B3178.key
cdo@desktop ~/src/cdo-guix-channel$ git add .
cdo@desktop ~/src/cdo-guix-channel$ git commit -S -m "Add cdo-358B3178.key"
```

Then we create a remote repo using SSH. This will be different depending on the
software running on the server. Here's how you'd do it for a bare `git
--daemon`:

```text
cdo@desktop ~/src/cdo-guix-channel$ ssh git@mutix.org -- git init /srv/git/cdo-guix-channel.git
cdo@desktop ~/src/cdo-guix-channel$ git remote add origin git@git.mutix.org:cdo-guix-channel.git
cdo@desktop ~/src/cdo-guix-channel$ git push --all origin
```

Make a note of the first signed commit to use in the next step as the 'channel
introduction' in the next step.

```text
cdo@desktop ~/src/cdo-guix-channel$ git log
commit 8802797eb54c7bd83be7540446fe2efd6ffb1cb8 (HEAD -> keyring, origin/keyring)
Author: Christina O'Donnell <cdo@mutix.org>
Date:   Sun Mar 3 17:49:24 2024 +0000

    Add cdo key.

commit 45db57dc9add2f175b23ec72c222915baafe879b (origin/master, master)
Author: Christina O'Donnell <cdo@mutix.org>
Date:   Sun Mar 3 17:36:57 2024 +0000

    Initial commit
```

Now write a `guix-channels.scm` on your machine to represent the channel:

<a id="org-example-block--guix-channels.scm"></a>
```text
;; ...

(define %cdo-local-channel
  (channel
   (name 'cdo)
   (url "file:///home/cdo/src/cdo-guix-channel")
   (introduction
    (make-channel-introduction
     "45db57dc9add2f175b23ec72c222915baafe879b"
     (openpgp-fingerprint
      "D899 861B 5EAD 198A CA06  2A9B 9DA1 DD52 53A7 AA4C")))))


(define %cdo-channel
  (channel
   (name 'cdo)
   (url "git@git.mutix.org:cdo-guix-channel.git")
   (introduction
    (make-channel-introduction
     "45db57dc9add2f175b23ec72c222915baafe879b"
     (openpgp-fingerprint
      "D899 861B 5EAD 198A CA06  2A9B 9DA1 DD52 53A7 AA4C")))))

(define-public %channels
  (list ;; ... Other channels
        %cdo-channel))

```

Next run `guix pull`:

```text
cdo@desktop ~/src/cdo-guix-channel$ guix pull -C guix-channels.scm
```

If this succeeds then you're all done.


### Troubleshooting {#troubleshooting}

If `guix pull` fails, then you may have to check the channel introduction, to see if
you copied all the values correctly.

If you get an error like this:

```text
guix pull: error: Git error: error authenticating: no auth sock variable
```

You'll need to have either `ssh-agent` or `gpg-agent` running in SSH mode.

I found that adding the home service [home-ssh-agent-service-type](https://guix.gnu.org/manual/devel/en/html_node/Secure-Shell.html) to my Guix Home
configuration and logging in again fixed it for me. If you're not using Guix
Home, then you'll want to start up `ssh-agent` and ensure that you've added the
key that you use for connecting to your remote.

Otherwise you can test it locally by using a `file://` URL, to diagnose the issue.


### Writing Package Definitions {#writing-package-definitions}

Once your Git repository is set up, it's time to write package
definitions. This can be accomplished by creating Guile modules that
export the packages you want to add to your channel. It's important to
note that the package definitions should be assigned to an exported
variable name using `define-public`. This assigns the package to a
variable so it can be referenced, even as a dependency of other
packages.

It's common practice to create your own top level directory where all your files
would sit. For example, all my packages are under a `cdo` directory:

```text
cdo/packages.scm
cdo/services.scm
cdo/config/channels.scm
cdo/config/desktop.scm
cdo/config/home.scm
cdo/config/system-common.scm
cdo/config/vps.scm
```


### Local File URL for Package Channel {#local-file-url-for-package-channel}

As mentioned above, if you're working on Guix packages on only one system, you
can use a local file URL (`file://`) for a package channel. This is useful when
developing on/using a channel.


## What's Next? {#what-s-next}

Congratulations, you are now a proud owner of your very own Guix channel! A
corner of the web to stash away your personal scripts and mods. Whether you've
chosen to keep it private or share it with the world, you can now write packages
in the confidence that if they work now, they'll still work in 20 years time.


## Recap {#recap}

We've discussed what a channel is, and what you can use it for. Then we used
GnuPG and Git to set up and sign the repo. Then we touched breifly on how you
can structure your guix channel. Finally we noted that you can set yourself up
to pull from `file://` URL's for development purposes.

Happy hacking!
