# dot-files

Wow.  dot files.  BFD.

## Software installations

### OpenBSD

- dwm -- build it yourself, so you can `#define MODKEY Mod4Mask`. (source: http://dwm.suckless.org)
- surf -- `pkg_add surf` will install surf 0.7 (latest) that was built with `static char *cafile = "/etc/ssl/cert.pem";` which is the correct value for OpenBSD.  Run `surf`, so that the `~/.surf` config directory can be created (if it didn't previously exist), and then, see `.surf/` (here) for extra config items. Add those; then run the following.
  - `(cd ~/.surf/styles; mv default.css default.css-orig; ln -sf inverted.css default.css)`
- tabbed -- `pkg_add tabbed`
- st -- `pkg_add st`
- other terminals -- `pkg_add sakura`, `pkg_add xfce4-terminal`. Others seem to "heavy" wrt dependencies (not already installed) and disk space.
