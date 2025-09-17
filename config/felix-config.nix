# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page, on
# https://search.nixos.org/options and in the NixOS manual (`nixos-help`).

{ config, lib, pkgs, ... }:
  
let 
  gnomeExts = with pkgs; [
    gnomeExtensions.hide-cursor
    gnomeExtensions.tiling-shell
  ];
   authorizedKeys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIE1/j87BAWauqvPy6ZHvEj6DEDXVTJquIPTMjWC3Lsff felix@nixos" 
	"ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQClZps7D/roOfL5TfWj0ZpkFoVtoDHqSku+68wwQHDJMZUSw6dy9N37iC66z4Q9B62rclZ8EDn4IljwwxDcslmNXwZDS/t7I4Qpfb+91RjMpwCEyvpdCuK2ziUtPsLOFuF8U3vCsorbcSKsOI1CC6w030iTH4RNA7P585fqgjhfbiPT36RrlyAuMWYR+Md4tqoItncOT2GJPDJWcapnaRAQQ1urHcd9/+2g3rlmA+KbbMyDFofkLYr3i8aPaP4kuTiS/ByYJGE1Rd4gITa7Y5WOwB8CmwbQ4sxMGWUcdTnHVTR+XLTMmytuPKOkDyCExBXm4kEioCh3BmDPHFXYDO/48wE6gXcH9r82+Xx8CFtUiWX+kZx90uoFlw0F4WHWMavS7Q1AiGmqK+9166VxqdP7PVVTB+bXprjYHJtRK/HLHTM/wPvaHn5o6Ob8CVGFoRxnlfaNUWQ0TQtpyQv3Mf/Sc2XELA0/0i4zUmYANKyPXjyay7HJaLwE/jPBLla28Qs= root@nixos"
	];
    stage1SSH = import ./stage1-ssh { inherit authorizedKeys; };
      mkApply = {
    apply ? x: x,
    attribute ? []
  }: { ... }: let
    attr = lib.setAttrByPath (attribute);
  in {
    options = attr (lib.mkOption {
      apply = v: apply v;
    });
  };
in {
  # used to disable the Obsidian cache so we don't need  '--option binary-caches...'  everywhere

  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./obsidian
      ./obsidian/users
      # disable cache
      (mkApply {
      attribute = [ "nix" "settings" "substituters" ];
      apply = old: lib.lists.subtractLists [
          "s3://obsidian-open-source"
          "http://obsidian.webhop.org:9150"
          "http://obsidian.webhop.org:8950"
          "http://obsidian.webhop.org:9050"
        ] old;
      })
    ];

   users.extraGroups.wheel.members = [ /* users who should have wheel access */ ];

#  boot = {
#    initrd = {
#      kernelModules = [ kernel_module_for_network_device ]; # Module name #should be in double quotes
#      network = stage1SSH;
#    }
#  };
  
  boot.initrd.luks.devices.root = {
    device = "/dev/disk/by-uuid/cee71050-378d-47cb-ae1a-3397123a47c6";
    allowDiscards = true;
  };

  networking.hostId = "68a81a5c";
  #hardware.nvidiaOptimus.disable = true;
  #services.xserver.videoDrivers = [ "i915" ];
  nixpkgs.config.allowUnfree = true;
  
  services.zfs.autoScrub = {
    enable = true;
    interval = "monthly";
  };
  #zfs set <poolname>/<dataset> compression=zstd
  users.users.felix = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable 'sudo' for the user.
  };
  
  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # networking.hostName = "nixos"; # Define your hostname.
  # Pick only one of the below networking options.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  # networking.networkmanager.enable = true;  # Easiest to use and most distros use this by default.

  # Set your time zone.
  # time.timeZone = "Europe/Amsterdam";

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";
  # console = {
  #   font = "Lat2-Terminus16";
  #   keyMap = "us";
  #   useXkbConfig = true; # use xkb.options in tty.
  # };

  # Enable the X11 windowing system.
  services.xserver.enable = true;


  # Enable the GNOME Desktop Environment.
  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  

  # Configure keymap in X11
  # services.xserver.xkb.layout = "us";
  # services.xserver.xkb.options = "eurosign:e,caps:escape";

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable sound.
  # services.pulseaudio.enable = true;
  # OR
  # services.pipewire = {
  #   enable = true;
  #   pulse.enable = true;
  # };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.alice = {
  #   isNormalUser = true;
  #   extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  #   packages = with pkgs; [
  #     tree
  #   ];
  # };

  # programs.firefox.enable = true;

  # List packages installed in system profile.
  # You can use https://search.nixos.org/ to find more packages (and options).
  environment.systemPackages = with pkgs; [
     vim # Do not add forget to an editor to edit configuration.nix! The Nano editor is also installed by default.
     wget
     firefox
     vscode
     zulip
     google-chrome
     gnome-software
     git
     # kindle
     spotify
     #opera
     # whatsapp
     # signal
     # linear
     # typst
     tinymist
     julia-bin
     rustc
     rustup
     rustfmt
     haskell.compiler.ghc967
     haskellPackages.cabal-install
     (haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
      aeson
      cassava
      colour
      diagrams-contrib
      diagrams-core
      diagrams-graphviz
      diagrams-lib
      diagrams-svg
      evdev
      extra
      fgl
      file-io
      generic-optics
      graphviz
      JuicyPixels
      lens
      lifx-lan
      lucid2
      massiv
      network
      optics
      optics-extra
      optparse-applicative
      optparse-generic
      pretty-simple
      process-extras
      safe
      servant
      servant-client
      servant-server
      shake
      wai
      warp
    ]))
    fourmolu
    haskell-language-server
     # haskellPackages.ShellFor
#     haskellPackages.ghcup
     # haskellPackages.stack
     # haskellPackages.hlint
     (pkgs.makeDesktopItem { name = "Gather"; desktopName = "Gather"; exec = "${pkgs.lib.getExe pkgs.chromium} --app=https://app.gather.town/app/BMa0PDnHghjBlmqU/obsidiansystems"; })
     (pkgs.makeDesktopItem { name = "Linear"; desktopName = "Linear"; exec = "${pkgs.lib.getExe pkgs.chromium} --app=https://linear.app/obsidiansystems/team/ARIA/active"; })
  ] ++ gnomeExts;
  
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;
  programs.ssh.startAgent = true;
  
  services.flatpak.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Copy the NixOS configuration file and link it from the resulting system
  # (/run/current-system/configuration.nix). This is useful in case you
  # accidentally delete configuration.nix.
  # system.copySystemConfiguration = true;

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "25.05"; # Did you read the comment?
 

}

