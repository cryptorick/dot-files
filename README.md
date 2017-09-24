# dot-files

Wow.  dot files.  BFD.

## Software installations

### OpenBSD

- suckless notes -- the default values of `X11INC = /usr/X11R6/include`, `X11LIB = /usr/X11R6/lib`, `CC = cc` are all OK for OpenBSD.
- dwm -- build it yourself (`git clone https://git.suckless.org/dwm`), so you can `#define MODKEY Mod4Mask`. Remember, in `Makefile`, to uncomment the line following the line that says `# OpenBSD (uncomment)`.  Heh. Now, you can build.
- surf -- `pkg_add surf` will install surf 0.7 (latest) that was built with `static char *cafile = "/etc/ssl/cert.pem";` which is the correct value for OpenBSD.  Run `surf`, so that the `~/.surf` config directory can be created (if it didn't previously exist), and then, see `.surf/` (here) for extra config items. Add those; then run the following.
  - `(cd ~/.surf/styles; mv default.css default.css-orig; ln -sf inverted.css default.css)`
- tabbed -- `pkg_add tabbed`
- st -- `pkg_add st`
- other terminals -- `pkg_add sakura`, `pkg_add xfce4-terminal`. Others seem too "heavy" wrt dependencies (not already installed) and disk space.
- extra fonts for terminals -- the next procedure will install those fonts into `~/.local/share/fonts`.
  - `git clone https://github.com/cryptorick/fonts.git powerline-fonts`
  - `cd powerline-fonts`
  - `git checkout obsd01` (This is my fix for `install.sh` to work on OpenBSD. Hopefully it will be accepted upstream.)
  - `sh install.sh`

### FreeBSD

- suckless notes -- the following `config.mk` items should be changed to these values.
  - `X11INC = /usr/local/include/X11`
  - `X11LIB = /usr/local/lib/X11`
