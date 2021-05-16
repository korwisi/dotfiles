# Kristof's Dotfiles

This is a bare gite repository for storing my dotfiles. Help yourself if it is of any use to you!

## Initialization

* Initialized with  `git init --bare $HOME/dotfiles`.
* Clone with `git clone --bare https://github.com/korwisi/dotfiles.git $HOME/dotfiles`.

## Configuration

* Add `alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'` to `.bashrc` (or alike file loaded upon login).
* Source .bashrc (or alike file).
* Configure repository to not show untracked files: `config config --local status.showUntrackedFiles no`


## Usage

* Pull file of repository: `config checkout master <file>`
* Add file to repository: `config add /path/to/file`
* Commit changes: `config commit -m "Whatever"`
* Push changes: `config push`
