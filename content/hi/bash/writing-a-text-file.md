---
title:                "एक लिखित फाइल लिखना"
html_title:           "Bash: एक लिखित फाइल लिखना"
simple_title:         "एक लिखित फाइल लिखना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Bash, also known as the Bourne-Again Shell, is a command-line interface that allows users to interact with their operating system through text commands. Writing a Bash script can greatly increase efficiency and automation in tasks, making it a valuable skill for any programmer or system administrator.

## How To

Writing a Bash script is relatively simple and only requires a text editor such as Atom or Notepad. Start by creating a new file with the extension ".sh". Then, every line in the script should begin with a "#", followed by the command or action. Here's an example:

```Bash
#!/bin/bash
#This line specifies the interpreter for the script
echo "Hello World!"
#This line prints "Hello World!" to the terminal
```
Sample output:
`Hello World!`

To run the script, navigate to the directory it is saved in and use the command `./scriptname.sh`. You can also add arguments to the script, making it more versatile. Here's an example of a script that takes in a name argument and prints a personalized message:

```Bash
#!/bin/bash
echo "Hello, $1!"
#The $1 represents the argument given after the script name when executing
```
Sample output:
`./scriptname.sh John` would print `Hello, John!`

## Deep Dive

Bash allows for conditional statements, loops, and other functions to make your scripts even more powerful. It also has built-in variables for things like the current date and time or the user's home directory. By adding these elements to your script, you can create dynamic and customizable outputs. For a more in-depth tutorial on Bash scripting, check out this guide: [https://www.tldp.org/LDP/abs/html/](https://www.tldp.org/LDP/abs/html/)

## आगे देखें (See Also)

[https://www.gnu.org/software/bash/](https://www.gnu.org/software/bash/)
[https://www.shellscript.sh/](https://www.shellscript.sh/)
[https://devhints.io/bash](https://devhints.io/bash)