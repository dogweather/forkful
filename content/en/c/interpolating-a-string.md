---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation is the process of substituting values of variables into placeholders in a string. Programmers do this to construct dynamic strings efficiently.

## How to:

In C, you can use sprintf function or printf function to achieve string interpolation. Here's a simple example using sprintf:

```C
#include <stdio.h> 

int main() 
{ 
    char buffer[50]; 
    int a = 10, b = 20; 

    sprintf(buffer, "Sum of %d and %d is %d", a, b, a+b); 
  
    printf("%s", buffer); 
  
    return 0; 
} 
```
This would output: `Sum of 10 and 20 is 30`.

You can use the printf function in a similar way:

```C
#include <stdio.h> 

int main() 
{ 
    int a = 10, b = 20; 
    printf("Sum of %d and %d is %d", a, b, a+b); 
  
    return 0; 
}
```
Again, this would output: `Sum of 10 and 20 is 30`.

## Deep Dive

Historically, C doesn't support string interpolation like modern languages such as Python, JS, or Swift. You have to use functions like sprintf or printf to mimic that behavior which is rather cumbersome.

As an alternative, you might consider using C++ with its powerful string manipulation methods, or a C extension like Objective-C that supports string interpolation with NSString class.

Technically, when you do string interpolation in C using sprintf or printf, it's formatting the string and printing it or storing it to a character array. Under the hood, these functions use variable argument lists (varargs) to accept any number of arguments after the format.

## See Also

For a deeper understanding, you may go through the following references:
1. [What is string Interpolation? - Stackoverflow](https://stackoverflow.com/questions/24538302/what-is-string-interpolation)
2. [C Programming/stdio.h/printf - Wikibooks](https://en.wikibooks.org/wiki/C_Programming/stdio.h/printf)
3. [Variable arguments in C - Cprogramming.com](https://www.cprogramming.com/tutorial/c/lesson17.html)