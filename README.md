# Kristof's Dotfiles

This is a bare gite repository for storing my dotfiles. Help yourself if it is of any use to you!

## Initialization

Initialized by  `git init --bare $HOME/dotfiles`

## Configuration

* Add `alias config='/usr/bin/git --git-dir=$HOME/dotfiles/ --work-tree=$HOME'` to `.bashrc`.
* Source .bashrc.
* Configure repository to not show untracked files: `config config --local status.showUntrackedFiles no`


## Usage

* Add file to repository: `config add /path/to/file`
* Commit changes: `config commit -m "Whatever"`
* Push changes: `config push`
