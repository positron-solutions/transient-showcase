## Pull Requests

See CONTRIBUTING.org for much more info on commit sign-off, GPG signing.  Here's
the reminders:

### Creating pull requests

1. Fork this repository into the personal GitHub account
1. Make changes on the personal fork
1. Remember to sign off and sign all commits in your PR branch (instructions in readme)
1. Make a Pull Request against this repository
1. **Allow maintainers to make changes to your pull request** (there's a
   checkbox)
1. Once the pull request has been approved, you will be thanked and observe your
   changes applied with authorship, signature, and sign-off in the git log

### Quality Assurance

- Don't break =org-babel-execute-source-block=
- Don't break compile or tangle for the package (`org-babel-tangle`)
- Don't break the TOC (use `org-make-toc` package)
- GPG sign your commits

CI & review are meant to raise quality.  Writing quality to begin with in every
change is always faster for everyone.

### Updating Nix Versions

This project usually should not update any nixpkgs versions frequently, but the
emacs overlay may benefit from updates.

Generated changes to the flake.lock should be kept in a separate commit for the
reviewer.  Title your commit as "generated".

Keeping these changes isolated in specific commits makes it much easier to pull
in your changes in parallel with other features.  Maintainers may harvest your
changes.  We only guarantee to preserve authorship, signature, and sign-off in
the git log.
