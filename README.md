# sysctl.el
View and edit sysctl in a hierarchal structure in Emacs.
Works on Linux, FreeBSD, OpenBSD, and MacOS.
Will work over TRAMP via SSH as well, including multiple hops.

![sysctl example](example.png)

## Commands
`sysctl` Generate the sysctl tree in a new buffer

## Keybindings
* `C-c C-c` Set the value of current position in the sysctl tree
* `C-c C-k` Refresh the value of current position in the sysctl tree
