---
title:                "C recipe: Finding the length of a string"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, there are certain tasks that seem simple but can be surprisingly complex. One of these tasks is finding the length of a string. While it may seem like a basic task, understanding how to properly find the length of a string is crucial for many programming tasks. In this blog post, we will explore the reasons why knowing how to find the length of a string is important, and how to do it in the C programming language.

## How To

When working with strings in C, it is often necessary to know the length of a particular string in order to manipulate it. In order to find the length of a string in C, we use the `strlen()` function. This function takes in a string as a parameter and returns the length of that string as an integer. Let's take a look at an example:

```C
#include <stdio.h>
#include <string.h>

int main()
{
  char str[] = "Hello World";
  int length = strlen(str);
  
  printf("The length of the string is %d", length);
  
  return 0;
}
```

In this example, we first include the necessary header files for using the `strlen()` function. We then declare a char array/string called `str` and assign it the value of "Hello World". Next, we declare an integer variable `length` and use the `strlen()` function to assign it the length of the string `str`. Finally, we print out the length using the `printf()` function.

The output of this code would be:

`The length of the string is 11`

As you can see, the `strlen()` function accurately calculates the length of the string.

## Deep Dive

Now, let's take a deeper dive into how the `strlen()` function actually works. It is important to note that the length of a string in C is determined by the number of characters before the terminating null character `'\0'`. This character indicates the end of a string in C. When `strlen()` is called, it starts at the first character of the string and counts each character until it reaches the terminating null character. This number is then returned as the length of the string.

It is also worth mentioning that the `strlen()` function is part of the standard C library and therefore is a part of every C compiler. This makes it a widely available and reliable option for finding the length of a string.

## See Also

Now that you know how to use the `strlen()` function in C, here are some resources for further reading:

- [C Documentation - String.h Library](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [C Programming Tutorial: String Length Function](https://www.programiz.com/c-programming/library-function/string.h/strlen)
- [Learn C Programming - Strings](https://www.learn-c.org/en/Strings)

By understanding the importance and proper implementation of the `strlen()` function, you can now confidently work with strings in your C programs. Happy coding!