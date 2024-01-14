---
title:                "C recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Command line arguments are a fundamental aspect of programming in the C language. They allow users to pass parameters to a program at runtime, providing a versatile and efficient way to interact with the program. As a C programmer, it is essential to understand how to read and process command line arguments to create robust and user-friendly programs.

## How To

To read command line arguments in C, we use the **argc** and **argv** parameters in the **main** function. **argc** stands for "argument count" and represents the number of command line arguments passed to the program. **argv** stands for "argument vector" and is an array of strings that contains the actual command line arguments.

Let's look at an example of a simple program that takes in two command line arguments and displays them on the screen:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
	// Check if two arguments are passed
	if (argc == 3) {
		// Display the first argument
		printf("First argument: %s\n", argv[1]);
		// Display the second argument
		printf("Second argument: %s\n", argv[2]);
	}
	else {
		// Display an error message
		printf("Invalid number of arguments. Please provide two arguments.\n");
	}

	return 0;
}
```

**Sample output:**

```
$ ./program arg1 arg2
First argument: arg1
Second argument: arg2
```

In the above example, we use the **argc** parameter to check if the user has provided the correct number of arguments. If so, we use **argv** to access the individual arguments and display them on the screen. It is essential to note that **argv[0]** will always contain the name of the program itself.

## Deep Dive

When working with command line arguments, it is crucial to consider potential errors and unexpected input from the user. For example, what if the user provides more or fewer arguments than expected? It is the programmer's responsibility to handle such scenarios gracefully.

One way to handle errors is to use conditional statements, as shown in the above example. However, there are other techniques such as using loops and the **strcmp()** function to compare strings. It is also possible to convert strings to numerical values using functions like **atoi()** and **atof()**.

Additionally, it is worth noting that the position and order of the command line arguments matter. For instance, if we swap the arguments in the above example, the output will also change. It is essential to consider these nuances when designing a program that relies on command line arguments.

## See Also

- [Command Line Arguments in C Programming](https://www.programiz.com/c-programming/c-command-line-arguments)
- [Using Command-Line Arguments in C/C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [C - Command Line Arguments](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)