---
title:    "C recipe: Printing debug output"
keywords: ["C"]
---

{{< edit_this_page >}}

## Why

When it comes to programming, debugging is an essential part of the development process. It involves finding errors and issues in a code that may cause the program to malfunction or fail. One of the most useful ways to debug a program is by using print statements. These statements allow you to print out values of variables and other data at different points in your code, which can help you pinpoint the source of a bug. In this blog post, we will discuss how to use printing for debugging in C programming.

## How To

To print debug output in C, you can use the `printf()` function from the standard input/output library. Here's an example of how to use it:

```
#include <stdio.h>

int main()
{
    int num = 10;
    
    printf("The value of num is: %d", num);
    
    return 0;
}
```

In this code, we have declared a variable `num` and assigned it a value of 10. We then use `printf()` to print the value of `num` using the placeholder `%d`, which is used for integers. The output of this code would be: `The value of num is: 10`.

You can also use multiple placeholders in one `printf()` statement to print multiple values. For example:

```
#include <stdio.h>

int main()
{
    int num1 = 10, num2 = 20;
    
    printf("The value of num1 is: %d and the value of num2 is: %d", num1, num2);
    
    return 0;
}
```

The output of this code would be: `The value of num1 is: 10 and the value of num2 is: 20`.

## Deep Dive

In addition to printing values of variables, you can also use `printf()` to print out messages or indicators at different points in your code. For example, you can print "Code reached this point" to indicate that a certain part of your code has been executed. This can be helpful in identifying which parts of your code are causing issues.

Another useful technique is to use conditional statements with `printf()`. For example, you can print out the values of variables only if certain conditions are met. This can be helpful in tracking the progress of your code and identifying where an error may occur.

It's important to note that excessive use of print statements can impact the performance of your program. Therefore, it's recommended to only use them for debugging purposes and remove them once the issue has been resolved.

## See Also

- [Using printf for Debugging in C](https://www.geeksforgeeks.org/using-printf-debugging-c/)
- [Debugging with printf Statements](https://www.tutorialspoint.com/cprogramming/c_debugging.htm)
- [Debugging Techniques in C](https://www.educative.io/blog/c-debugging-techniques)