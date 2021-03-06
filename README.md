# git-sanity

This tool allow you sanity check the history of local git repository.

A 'sane' history is one where there is no interleaved merged.

## Install

`git-sanity` is published on [hackage](http://hackage.haskell.org/package/git-sanity).

    cabal install git-sanity

Note: You might have to install binary dependencies manually as cabal does not support them yet!

## Usage

    git-sanity check [<revision range>]

* **check**: Check the whole history leading to the current commit.
* **check \<revision range\>**: Check only commits in the specified revision range.

## Git hook

In order to integrate nicely as a pre-push githooks (http://git-scm.com/docs/githooks.html), a revision range of `origin/$branch~1..HEAD` can be used.

Here is an example `.git/hooks/pre-push` file:

    #!/bin/sh

    branch=$(git symbolic-ref --short HEAD)

    git-sanity check origin/$branch~1..HEAD

Note: The hook script `pre-push` must have execution permission to be successfully called by git (`chmod +x .git/hooks/pre-push`)
