---
title:    "C recipe: Reading command line arguments"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you’re new to C programming, you may be wondering why you would need to read command line arguments. Well, command line arguments allow your program to receive information from the user at runtime, making your program more dynamic and interactive.

## How To

Reading command line arguments in C is actually quite simple. Let’s take a look at an example:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
  // argc represents the number of arguments
  // argv is an array of strings that contains the arguments
  printf("Number of arguments: %d\n", argc);

  // Loop through the arguments and print them out
  for (int i = 0; i < argc; i++) {
    printf("Argument %d: %s\n", i, argv[i]);
  }
  
  return 0;
}
```

Let’s compile this code using gcc and run it with two arguments:

```bash
gcc command_line_args.c -o command_line_args
./command_line_args Hello World
```

The output should look something like this:

```
Number of arguments: 3
Argument 0: ./command_line_args
Argument 1: Hello
Argument 2: World
```

As you can see, the first argument in the `argv` array is always the name of the program itself. The rest of the arguments are passed in by the user.

## Deep Dive

Now, let’s take a closer look at the command line arguments. When using the `argc` and `argv` variables, it’s important to understand that the arguments are represented as strings. This means that any numerical values or characters will need to be converted to the appropriate data type. Additionally, it’s important to check for the correct number of arguments and handle any errors gracefully.

Another useful feature of command line arguments is the ability to pass in flags or options. These are typically preceded by a dash (`-`) and can be used to modify the behavior of the program. For example, we can modify our previous example to only print out the second argument if a flag is passed in:

```C
#include <stdio.h>
#include <string.h>

int main(int argc, char *argv[]) {
  // Check for the flag
  if (strcmp(argv[1], "-c") == 0) {
    printf("Second argument: %s\n", argv[2]);
  }
  
  return 0;
}
```

Now when we run our program with the `-c` flag, we should only see the second argument printed out:

```
Number of arguments: 3
Second argument: World
```

## See Also

For more information on reading command line arguments in C, check out these resources:

- [Command Line Arguments in C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [The GNU C Library: Program Arguments](https://www.gnu.org/software/libc/manual/html_node/Program-Arguments.html#Program-Arguments)
- [C Command Line Arguments Tutorial](https://www.learn-c.org/en/Command_Line_Arguments)