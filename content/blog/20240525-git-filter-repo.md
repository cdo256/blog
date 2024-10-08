+++
title = "How to Erase Secrets in Git Repos with git filter-repo"
author = ["Christina O'Donnell"]
date = 2024-05-25
linkTitle = "How to Erase Secrets in Git Repos with git filter-repo"
draft = false
[menu]
    [menu.main]
        parent = "blog"
+++

Here's the scenario: you have your dotfiles in a private repo that you've been
maintaining for years, and after a lot of polishing and removing secrets, you're
ready to share publicly on your trophy list that is your GitHub/GitLab/SourceHut
profile page.

There's only one problem: You never checked to see whether there were any
passwords or other secrets in your history. Now, hopefully you remembered before
you made the repo public because there are [innumerable bots](https://trufflesecurity.com/blog/thousands-of-github-comments-leak-live-api-keys) scraping git hosting
platforms for passwords, API-keys, and other secrets.&nbsp;[^fn:1]


## Installation {#installation}

In this article, we'll be using [Gitleaks](https://github.com/gitleaks/gitleaks) to detect secrets in a repo, and `git
filter-repo` to remove it without losing important history.

You can usually install both of these using your favorite package manager.
[^fn:2] See Repology for
[gitleaks](https://repology.org/project/gitleaks/versions) and [git-filter-repo](https://repology.org/project/git-filter-repo/versions).

For more on Git security and rewriting history, see [Pro Git - Git Security](https://git-scm.com/book/en/v2/Git-Tools-Rewriting-History#_git_rewriting_history).


## Finding secrets {#finding-secrets}

Gitleaks is a powerful Static Application Security Testing (SAST) tool designed
to detect and prevent hardcoded secrets like passwords, API keys, and tokens in
git repositories. It's an essential tool for developers and security teams to
safeguard sensitive information and comply with security best practices. This
guide will walk you through the basics of using Gitleaks, from installation to
running scans on your repositories. For secrets management best practices, refer
to the [OWASP Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Secrets_Management_Cheat_Sheet.html).

Before we start, it is recommended to have a fresh clone:

```sh
git clone --mirror YOUR_REPO /tmp/clone
cd /tmp/clone
```

Then run the below to search for unwanted secrets:

```sh
gitleaks detect --verbose --source .
```

This will give a report that will show an output of the different Git commit
hashes, file-paths and the secret at that location. Keep in mind that these are
just strings that look like they could be secret. Not every one of these will
actually be a key, and Gitleaks won't necessarily catch all secrets you'd want
to keep private.

If there's a large output, or if you're looking to automate the detection
process (eg. as a push hook), then it can be set up to generate CSV or JSON
reports like so:

```sh
gitleaks detect --source . --report=report.json
```

This will produce a json file which can be read by a script.

Once you've identified the secrets you'd like to erase, we can move on to using
`git filter-repo`.


## Removing secrets {#removing-secrets}

`git filter-repo` is a versatile tool that replaces the older `git
filter-branch` and BFG Repo-Cleaner for rewriting Git history. It allows you to
modify your repository's history, including removing files, replacing text
within files, and even changing commit metadata. `git filter-repo` is designed
to be safer and faster than its predecessors, making it the recommended tool for
history rewriting tasks. For detailed documentation, see [git filter-repo
Documentation](https://github.com/newren/git-filter-repo/blob/main/Documentation/git-filter-repo.txt).

As mentioned above, you'll want to use a fresh clone of the repository you
intend to modify. This is because `git filter-repo` works by rewriting the
**entire repo**, so the usual guarantees of being able to fish out the old version
with [`git reflog`](https://git-scm.com/docs/git-reflog) and the like. Your repo will be rewritten in its entirety.

There are different commands, depending on the type of secret you'd like to
remove. If you'd like to remove a private key file, for example then you'd want
to use --path to remove the file entirely. Otherwise, if it's just a single-line
API key or password, then you can use --replace-text to remove it.


### Removing a file {#removing-a-file}

To remove a file from your entire repository history, use the following
command:

```sh
git filter-repo --invert-paths --path path/to/secret/file
```

Replace `path/to/secret/file` with the actual path to the file you want to
remove. The `--invert-paths` option tells `git filter-repo` to remove the
specified file(s), because by default it will keep only the paths mentioned,
which is the opposite of what we want.


### Removing secrets in text {#removing-secrets-in-text}

If you need to remove secrets from within files without deleting the
files themselves, use the `--replace-text` option. First, create a file
named `expressions.txt` and list the secrets you want to replace,
followed by `==>REPLACEMENT`, where `REPLACEMENT` is what you want to
replace the secret with (often something generic like `REMOVED`).

```text
SECRET_jasdg021h0A7098==>REMOVED
OOPS_P455W0RD==>REMOVED
```

Be careful not to replace quotes if you want the repo to still compile.

Next run:

```sh
git filter-repo --replace-text expressions.txt
```

After rewriting history, you'll need to force push the changes to your
remote repository:

```sh
git push origin --force --all
git push origin --force --tags
```

This step overwrites the history on the remote repository with your
modified history. Be cautious, as this action cannot be undone.

Rewriting history can create coordination headaches on popular projects, because
everyone's local branch will have to be updated to have the new history. The
first step will be to inform any collaborators to re-clone the repository, as
their local histories will now be incompatible with the rewritten history.

And if you haven't already, you'll need to change/invalidate all passwords/API
keys that were on the repo. This is good practice even if the repo hasn't been
made public yet, in case someone retains a copy with the old history.

For more information on verifying the removal of sensitive data, consult this
Stack Overflow discussion: [Stack Overflow - Removing Sensitive Data](https://stackoverflow.com/questions/5302520/how-do-i-verify-removal-of-sensitive-data-from-a-git-repository).


## Conclusion {#conclusion}

`git filter-repo` is a powerful tool for removing sensitive data from
Git repositories. By following the steps outlined in this article, you
can ensure that secrets are erased from your repository's history,
helping to maintain the security of your project. Remember, the best
practice is to avoid committing sensitive information in the first
place, but if mistakes happen, `git filter-repo` provides a reliable way
to rectify them.

[^fn:1]: If not, run GitLeaks
    and change those secrets as soon as possible. If you're lucky, then no one will
    have used up your AWS credits mining crypto!
[^fn:2]: Unfortunately not Guix yet.
