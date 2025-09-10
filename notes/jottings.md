jottings

The obsidian module will generate an ssh key at `/etc/obsidian/ssh_deploy_key`. To allow nixbld users to access private O.S. repos, you must provide the repo admin the public key located at `/etc/obsidian/ssh_deploy_key.pub`.


switch --option binary-caches ""

https://gitlab.com/obsidian.systems/nixos-configuration/-/merge_requests/115

waiting on:
- PR merge to add self user as
nix-shell -p google-chrome --option binary-caches "https://cache.nixos.org"

sudo nixos-rebuild switch --option binary-caches "https://cache.nixos.org"

three fingers to copy between windows
or glide up on trackpad, repeatedly

flatpak: run Docker images
https://flathub.org/setup/NixOS

Gather runs ok on Chrome
	but on Firefox no video somehow

The function will not be run in future, but you can run
it yourself as follows:
  autoload -Uz zsh-newuser-install
  zsh-newuser-install -f

cleaning up NixOS with garbage collection:
 sudo nix-env --delete-generations old
 sudo nix-store --gc

once flatpak is working:
installed Katvan a Typst editor
saved file as .xdp-test-text.typ-VbP29r 
because obviously you would
installed VLC, can play movies? yes. although there was an adequate default player.

	
	
