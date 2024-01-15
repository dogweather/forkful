---
title:                "Starting a new project"
html_title:           "Fish Shell recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Why
Starting a new project can be an exciting and daunting task. Whether it's for personal use or for a company, using the Fish shell can make the development process smoother and more efficient. With its user-friendly syntax and powerful features, Fish shell is a great tool to help you kickstart your project.

## How To
To start a new project using Fish shell, all you need to do is follow these simple steps:
1. Open your terminal and navigate to the location where you want to create the project.
2. Type `mkdir <project-name>` to create a directory for your project.
3. Next, use the `cd` command to enter the project directory.
4. Now, you can start writing your code using your favorite text editor.
5. To execute your code, use `fish <file-name>` in the terminal.

Here's an example of how to create a new project called "my-awesome-app" and run a simple "Hello World" program using Fish shell:

```
Fish Shell Project
$ mkdir my-awesome-app
$ cd my-awesome-app
Fish Shell Project/my-awesome-app $ echo "Hello World"
Hello World
Fish Shell Project/my-awesome-app $ fish hello_world.fish
Hello World
```

## Deep Dive
When starting a new project with Fish shell, there are a few things to keep in mind:
- Fish shell has its own set of built-in functions and variables, making it different from other shells. It's important to familiarize yourself with these to make the most out of the shell.
- Fish shell has auto-suggestion feature, which can be helpful when writing commands. Just press the right arrow key to accept the suggestion.
- To customize your Fish shell, you can create a `config.fish` file in your project directory and add your desired configurations.

See Also
- Official Fish Shell website: https://fishshell.com/
- Fish shell tutorial: https://www.digitalocean.com/community/tutorials/an-introduction-to-the-fish-shell
- List of built-in functions and variables for Fish shell: https://fishshell.com/docs/current/commands.html

Now that you know how to effectively start a new project using Fish shell, go ahead and give it a try for your next project. Happy coding!