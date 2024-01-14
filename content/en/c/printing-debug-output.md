---
title:                "C recipe: Printing debug output"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why 

Have you ever encountered a bug in your code and spent hours trying to figure out what went wrong? Debug output can be a lifesaver in these situations. By printing out specific variables or messages during program execution, you can gain insights into the code and identify the root cause of the bug.

## How To 

Printing debug output in C is a simple process. You can use the `printf()` function to display the value of a variable or a message on the console. Let's take a look at a simple example:

```C
#include <stdio.h>

int main()
{
  int num1 = 10;
  int num2 = 5;

  printf("The value of num1 is %d\n", num1);
  printf("The value of num2 is %d\n", num2);

  return 0;
}
```

Output:

```
The value of num1 is 10
The value of num2 is 5
```

In the above code, we use the `%d` format specifier to print the value of the `num1` and `num2` variables. You can also use other format specifiers according to the data type of the variable, such as `%f` for floating-point numbers and `%c` for characters.

Besides displaying variable values, you can also print out messages to provide additional information about the program's execution. For example:

```C
#include <stdio.h>

int main()
{
  int age = 25;

  if(age >= 18)
    printf("You are eligible to vote!\n");
  else
    printf("You are not eligible to vote yet.\n");

  return 0;
}
```

Output:

```
You are eligible to vote!
```

## Deep Dive 

There are a few best practices to keep in mind when using debug output in your C programs. 

First, it's essential to use meaningful variable names. This makes it easier to understand the output and reduces confusion. Additionally, you can use comments to explain the purpose of each `printf()` statement.

Second, you can use conditional statements to control when debug output is displayed. This way, you can turn off the debug output when you no longer need it.

Lastly, remember to remove all debug output statements before releasing your code. Leaving them in the final version can impact performance and lead to cluttered code.

## See Also 

- [Debugging Guide for C Programmers](https://www.learnc.org/c-programming/debugging)
- [The Importance of Good Variable Naming in C](https://medium.com/swlh/the-importance-of-good-variable-naming-in-c-f58e175f6939)
- [Tips for Debugging in C](https://barrgroup.com/embedded-systems/how-to/debugging-c-program-cortex-m)

By using these techniques, you can effectively use debug output to troubleshoot your code and streamline the debugging process. Happy coding!