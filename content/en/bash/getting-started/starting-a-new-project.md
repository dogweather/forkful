---
date: 2024-01-20 18:03:07.866589-07:00
description: 'How to: Let''s create a simple script to bootstrap a new project.'
lastmod: '2024-03-13T22:45:00.245427-06:00'
model: gpt-4-1106-preview
summary: Let's create a simple script to bootstrap a new project.
title: Starting a new project
weight: 1
---

## How to:
Let's create a simple script to bootstrap a new project.

```Bash
#!/bin/bash

# Project setup script

PROJECT_NAME=$1
BASE_DIR=$(pwd)

# Function to create directories
make_directories() {
    mkdir -p $PROJECT_NAME/{bin,src,doc,test}
    echo "Directories created."
}

# Function to create initial files
make_files() {
    touch $PROJECT_NAME/README.md
    touch $PROJECT_NAME/src/main.sh
    echo "#!/bin/bash" > $PROJECT_NAME/src/main.sh
    chmod +x $PROJECT_NAME/src/main.sh
    echo "Initial files created."
}

# Function to initialize a git repository
init_git() {
    cd $PROJECT_NAME
    git init
    cd $BASE_DIR
    echo "Git repository initialized."
}

# Main execution
if [ -z "$PROJECT_NAME" ]; then
    echo "Please specify a project name."
else
    make_directories
    make_files
    init_git
    echo "Project '$PROJECT_NAME' created."
fi
```
Sample output after running `bash setup.sh myproject`:

```Bash
Directories created.
Initial files created.
Initialized empty Git repository in /path/to/myproject/.git/
Project 'myproject' created.
```

## Deep Dive
Before we had scripts, we'd manually create directories and files every time—tedious and error-prone. Automation with a script minimizes mistakes and speeds things up.

Alternatives include tools like Yeoman, which scaffolds projects in various languages, but that's like using a power drill when you need a thumbtack.

The script above is simple on purpose. It makes a project directory, subdirectories for organization (like `src` for source code), and essential files (like `README.md`). Plus, it sets up a Git repo so you can save versions of your work. You can tweak and add to it for each project's needs.

## See Also
- Git documentation: https://git-scm.com/doc
- Yeoman: http://yeoman.io/
- Bash scripting tutorials: https://www.shellscript.sh/
