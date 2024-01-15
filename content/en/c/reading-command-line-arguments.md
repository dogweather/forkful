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

## Why

Curious about how to make your C programs more flexible? One way is to utilize command line arguments, which allows users to give inputs when running the program. It can greatly enhance your program's capabilities and make it more user-friendly.

## How To

To start, we need to declare the main function with two parameters, `argc` and `argv`. These represent the number of arguments and the actual arguments given by the user, respectively. 

```C
int main(int argc, char *argv[]) {
  // code goes here
}
```

Now, let's take a look at how we can access and use these command line arguments. The first argument, `argv[0]`, is always the name of the program itself. The rest of the arguments can be accessed using their corresponding indices. For example, if the user runs the program with `./myprogram arg1 arg2`, then `argv[1]` will contain `arg1` and `argv[2]` will contain `arg2`.

```C
// program to print out all the command line arguments
#include <stdio.h>

int main(int argc, char *argv[]) {
  for (int i = 1; i < argc; i++) {
    printf("Argument %d: %s\n", i, argv[i]);
  }
  return 0;
}
```

**Sample Output:**

```
Argument 1: arg1
Argument 2: arg2
```

It's important to note that the arguments are always read as strings, so you may need to convert them to the appropriate data type if needed.

## Deep Dive

Now, let's dive a bit deeper into reading command line arguments. You may be wondering what the `argc` parameter is for. It is simply counting the number of arguments passed in, including the program name itself. So in the above example, `argc` would have a value of 3.

Another useful function to know is `atoi()`, which converts a string to an integer. This can be helpful if you want to perform mathematical operations on the command line arguments.

```C
// program to find the sum of two command line arguments
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
  int num1 = atoi(argv[1]);
  int num2 = atoi(argv[2]);
  int sum = num1 + num2;
  printf("Sum of %d and %d is %d\n", num1, num2, sum);
  return 0;
}
```

**Sample Output:**

```
Sum of 10 and 20 is 30
```

You can also use nested loops or conditional statements to perform different actions based on the arguments given by the user.

## See Also

- [C Command Line Arguments](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Taking Command Line Arguments in C](https://www.tutorialspoint.com/taking-command-line-arguments-in-c-cplusplus)
- [How to Use Command Line Arguments in C Programs](https://www.dummies.com/programming/c/how-to-use-command-line-arguments-in-c-programs/)