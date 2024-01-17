---
title:                "Reading command line arguments"
html_title:           "Bash recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments is a way for Bash programmers to pass input to their script or program from the command line. This allows for more flexibility and customization, as the user can specify different values or options when executing the script.

## How to:

To read command line arguments in a Bash script, you can use the $1, $2, and so on variables to represent the arguments passed in. For example, if your script is named "myScript.sh" and you want to pass in two arguments, you would use the following command:

```
$ bash myScript.sh 123 "Hello"
```

Inside your script, you can access the arguments as follows:

```
#!/bin/bash
echo "The first argument is: $1"
echo "The second argument is: $2"
```

The output of this script would be:

```
The first argument is: 123
The second argument is: Hello
```

## Deep Dive

Before reading command line arguments became a common practice, Bash programmers had to rely on hardcoded values within their scripts. This made it difficult to make their scripts more dynamic and customizable for different use cases.

There are a few alternative methods to reading command line arguments, such as using environmental variables or making use of the "shift" command. However, these methods may not always be as straightforward as simply using the $1, $2 variables.

When implementing the ability to read command line arguments, it is important to note that the arguments are separated by spaces, so it is best to be mindful of any arguments that may contain spaces themselves. This can be accounted for by using quotation marks around the arguments when executing the script.

## See Also

- [Bash Guide for Beginners - Command Line Arguments](http://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)
- [Bash Accessories - Command Line Arguments](http://tldp.org/LDP/abs/html/bashver4.html#COMMANDLINEARGS)