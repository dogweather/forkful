---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Start a New Project Using Fish Shell

## What & Why?

Starting a new project means creating a distinct workspace and files for a specific software or app development. It helps keep your work organized and separate from other tasks.

## How to:
Open up your terminal and let's roll:

Check the current installed version
```fish
fish --version
```
You will get output similar to this
```fish
fish, version 3.2.2
```

Now, set up a new project:
```fish
mkdir my_fish_project
cd my_fish_project
touch Main.fish
```

Set up Fish as your default shell:
```fish
chsh -s /usr/local/bin/fish
```
Restart your terminal and check your default shell using the command:
```fish
echo $SHELL
```
It should show `/usr/local/bin/fish`.

## Deep Dive

Fish Shell or "friendly interactive shell" is an intuitive and user-friendly shell. Released in 2005, it's not installed by default in UNIX-based systems, but it can be installed alongside other shells. It differed from Bash, the default shell in many UNIX systems, by focusing on interactive user experiences and plain English commands.

If you prefer, alternatives to Fish exist. These include Zsh, which also emphasizes improved user experience, and Bash, the omnipresent and proven option. 

While starting a new project isn't technical, certain things run in the background when you do so in Fish Shell. When you set Fish as your default shell, the system changes your default shell pointer in `/etc/passwd` file. 

Fish doesn't support POSIX, the compliance standard for shells, fully. However, it does so enough to be reliable in a productive system. When a new project is initiated, it's placed as a working directory in Fish's stack, providing structure and continuity. 

## See Also
[Official Fish Documentation](https://fishshell.com/docs/current/index.html)

[A comparison of Bash, Zsh, and Fish](https://www.slant.co/versus/167/169/~bash_vs_zsh_vs_fish)