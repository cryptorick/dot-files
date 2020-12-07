{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = builtins.getEnv "USER";
  home.homeDirectory = builtins.getEnv "HOME";

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    tmux
    mg

    #fonts
    iosevka
    d2coding  # like iosevka
    hack-font
    terminus_font
    terminus_font_ttf
    cantarell-fonts
    #hermit
    #ubuntu-font-family  # I guess Ubuntu Mono is in this.
  ];

  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      vterm
    ];
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.03";
}
