---
title:                "Reading command line arguments"
html_title:           "C recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments in C is the process of gathering information provided by the user through the command line when executing a program. This can include things like file names, options, or other necessary parameters. It allows programmers to make their programs more dynamic and customizable, as well as enabling them to create more efficient and user-friendly interfaces.

## How to:

Reading command line arguments in C is a fairly straightforward process. It involves taking in the arguments from the command line when calling the program and storing them in variables to be used in the program.

Here's a simple example of a program that takes in two string arguments and prints them out:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    //argc contains the number of arguments passed in (including the program name itself)
    //argv is an array of strings, with each element containing an argument
    if (argc != 3) { //check if the correct number of arguments were provided
        printf("Incorrect number of arguments provided. Please provide two strings.\n");
        return 1; //exit the program with an error
    }
    //print out the two arguments provided
    printf("First argument: %s\n", argv[1]);
    printf("Second argument: %s\n", argv[2]);

    return 0;
}
```

Sample output:

```
$ ./program hello world
First argument: hello
Second argument: world
```

## Deep Dive

Command line arguments have been a part of C since its early days and have become a staple in many popular C programs. They allow for a level of flexibility and control that is beneficial for both programmers and end users.

Alternative methods for gathering user input include using input prompts within the program or reading from external files. However, command line arguments are often preferred due to their convenience and ease of use.

In terms of implementation, command line arguments are passed to the program through the operating system when it is called. In the example above, `argc` gives the number of arguments, while `argv` is an array of strings containing the actual arguments.

## See Also

To learn more about working with command line arguments in C, check out the following resources:

- [C Programming - Command Line Arguments](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [Command Line Arguments in C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [glibc - Command-Line Arguments (GNU C Library)](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html)