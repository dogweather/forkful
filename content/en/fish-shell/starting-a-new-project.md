---
title:                "Fish Shell recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why
Starting a new project can seem overwhelming, but with the help of the Fish Shell, it doesn't have to be. By using this powerful and user-friendly shell, you can easily navigate and execute commands in a more efficient and intuitive way.

## How To
To get started, first make sure you have the Fish Shell installed on your system. If you are using a Mac, you can download and install it using Homebrew with the command:

```Fish Shell
brew install fish
```

Once you have Fish Shell installed, you can start using it by opening a terminal and typing the command `fish`. This will launch the Fish Shell and you can begin navigating and executing commands.

To create a new project, you can use the `mkdir` command to make a new directory and then use `cd` to navigate into that directory. For example:

```Fish Shell
mkdir my_project
cd my_project
```

Next, you can use the `touch` command to create new files within the project. Then, open the files with your preferred text editor and start coding! For example:

```Fish Shell
touch index.html style.css
```

This will create two new files within the `my_project` directory. You can then use the `nano` command to open the files in the nano text editor:

```Fish Shell
nano index.html
nano style.css
```

## Deep Dive
The power of the Fish Shell comes from its extensive and customizable auto-complete feature. This means that as you type out commands or file names, you can use the tab key to automatically fill in the rest of the word or suggest options. This can save you time and reduce errors in your coding.

Additionally, you can use the `alias` command to create shortcuts for commonly used commands. For example, if you frequently use the `git add` and `git commit` commands, you can create an alias for them like this:

```Fish Shell
alias ga="git add"
alias gc="git commit"
```

This allows you to simply type `ga` or `gc` instead of the longer command, making your workflow more efficient.

## See Also
To learn more about Fish Shell and how to optimize your experience with it, check out these helpful resources: 

- [Fish Shell homepage](https://fishshell.com/)
- [Fish Shell tutorial](https://www.youtube.com/watch?v=uImIqB9xcBk)
- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)

Now that you know the basics of starting a new project with the Fish Shell, go ahead and give it a try! You'll see just how easy and convenient it can make your coding experience. Happy coding!