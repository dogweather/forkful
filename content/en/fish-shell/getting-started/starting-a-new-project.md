---
date: 2024-01-20 18:03:14.094776-07:00
description: "Starting a new project means initializing a fresh directory with everything\
  \ you need to get coding. Programmers do this to kick-off development in a\u2026"
lastmod: '2024-03-13T22:45:00.474876-06:00'
model: gpt-4-1106-preview
summary: Starting a new project means initializing a fresh directory with everything
  you need to get coding.
title: Starting a new project
weight: 1
---

## What & Why?
Starting a new project means initializing a fresh directory with everything you need to get coding. Programmers do this to kick-off development in a clean, organized way.

## How to:
```fish
# Create a new directory and enter it
mkdir my_fish_project
cd my_fish_project

# Initialize a git repository
git init

# Create an initial commit with a .gitignore file
echo "*.log" > .gitignore
git add .gitignore
git commit -m "Initial commit with .gitignore"

# Bonus: Set up a virtual environment if applicable (not native to Fish or git)
# Make sure to have a virtual environment tool installed.
```
Sample output:
```
Initialized empty Git repository in /path/to/my_fish_project/.git/
[master (root-commit) abc1234] Initial commit with .gitignore
 1 file changed, 1 insertion(+)
 create mode 100644 .gitignore
```

## Deep Dive
The practice of setting up a new project has a long lineage, becoming more standardized with the rise of modern version control like Git. While some may use more graphical approaches, command-line lovers prefer the fine control and speed of terminal commands. Fish Shell, known for its user-friendly design, makes it simpler with helpful features like syntax highlighting and autocompletions.

Alternatives include using IDEs with built-in project initialization or scripts in other shells like Bash or Zsh — but Fish shines in its simplicity and interactivity. When it comes to implementation, the init process is inherently customizable; you adapt it to fit the stack and toolchain of your choice. Whether it's adding build tools, setting up linters, or creating a directory structure, it's all about making your future development smoother.

## See Also
- Fish Shell Documentation: https://fishshell.com/docs/current/index.html
- Git Basics: https://git-scm.com/book/en/v2/Getting-Started-Git-Basics
- Setting up Virtual Environments: https://virtualfish.readthedocs.io/en/latest/index.html
