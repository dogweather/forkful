---
title:                "C recipe: Reading command line arguments"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Reading command line arguments is an important aspect of C programming, especially for those who want to create command line tools or programs that require user input. By learning how to read command line arguments, you can enhance the functionality of your programs and make them more user-friendly.

## How To

To read command line arguments in C, you will need to use the main() function, which is the starting point of any C program. The main() function takes two parameters, argc and argv, which stand for argument count and argument vector respectively.

```C
int main(int argc, char *argv[])
```

The argc parameter holds the number of arguments passed to the program from the command line, including the program name itself. The argv parameter is an array of strings that contains the actual arguments passed to the program.

Let's take a look at a simple example:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    for(int i=0; i<argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

In this example, we use a for loop to iterate through the arguments and print them out with the corresponding index number. The output will be:

```
./program_name
Argument 0: ./program_name
```

If we run the program with additional arguments, such as `./program_name hello world`, the output will be:

```
./program_name hello world
Argument 0: ./program_name
Argument 1: hello
Argument 2: world
```

You can also use command line arguments to pass data into your program. For example, if you want to read a file name from the command line, you can use `argv[1]` to access the first argument after the program name. Note that the arguments passed to the command line are always treated as strings, so you may need to convert them to the appropriate data type if necessary.

## Deep Dive

In addition to the argc and argv parameters, the main() function can also take a third parameter, envp, which represents the program's environment variables. These variables can be used to pass additional information from the command line to the program.

It's important to note that the order of the arguments and environment variables may vary depending on the operating system. For example, on Windows systems, the arguments are separated by spaces, while on Unix systems, they are separated by spaces or tabs.

There are also libraries, such as `getopt` and `argp`, that provide more advanced options for parsing command line arguments. These libraries can handle different argument formats and provide features like error handling and help messages.

## See Also

- [C Programming Tutorial - Command Line Arguments](https://www.programiz.com/c-programming/c-command-line-arguments)
- [The Linux Command Line: Arguments](https://linuxcommand.org/lc3_lts0080.php)
- [C Standard Library - Environment Variables](https://en.cppreference.com/w/c/program/argv)