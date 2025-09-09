# Starting a Nix Shell
```
nix-shell --option binary-caches 'https://cache.nixos.org'
```

# Setting up Visual Studio Code

```
code . &
```

Install the Haskell Language Support plugin
In the plugin's Settings,  set the 'Manage HLS' value to
PATH rather than GHCUp.
(Can fix it to automatically use the nix-shell? Document here)
 
