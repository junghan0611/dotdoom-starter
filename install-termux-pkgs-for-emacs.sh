#!/usr/bin/env sh

pkg update && pkg upgrade -y
# termux-change-repo

pkg install curl wget file git direnv openssh neovim byobu gh ripgrep ripgrep-all zsh fortune man -y
pkg install sqlite mlocate git-delta fd tree rlwrap ack-grep cmake fasd libsqlite -y
pkg install python python-pip nodejs -y
pkg install mcfly shfmt libvterm libtool ncdu -y
pkg install hunspell aspell -y
pkg install pandoc stow -y
pkg install libtreesitter typst -y
pkg install elixir -y
pkg install translate-shell -y
pkg install editorconfig shellcheck tidy gnuplot libxml2-utils -y
pkg install neofetch imagemagick ffmpeg -y
pkg install enchant enchant-static -y

# 2024-02-24 xclip-mtehod termux-clipboard-get
pkg install termux-api -y
termux-setup-storage

pkg install emacs -y
# pkg install openjdk-17 -y

git config --global core.quotepath false

# symlink
cd ~
ln -s /data/data/com.termux/files/ root
ln -s /storage/emulated/0/Documents/sync sync

# ssh ip -p 8022

# hunspell
# ln -s ~/.spacemacs/.hunspell_ko_personal ~/.hunspell_personal
# cp hunspell/* ~/root/usr/share/hunspell/

date
echo -e "Update Done\n"
