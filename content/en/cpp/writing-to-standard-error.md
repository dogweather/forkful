---
title:    "C++ recipe: Writing to standard error"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

In C++ programming, writing to standard error is a common practice for displaying error messages and debugging code. It allows developers to easily track and handle any errors that may occur during program execution.

## How To 

To write to standard error in C++, you will first need to include the <iostream> header file. Then, use the "cerr" object to write the error message followed by the << operator. Here's an example code:

```C++
#include <iostream>
using namespace std;

int main() {
  int num = 5;
  if(num < 10) {
    cerr << "Error: Number is less than 10." << endl;
  }
  return 0;
}
```

The output of this code will be: "Error: Number is less than 10." displayed in the console for the developer to see. It is important to use the "endl" operator at the end of the error message to ensure it is displayed properly on the next line.

Another way to write to standard error is by using the "fprintf" function. This allows for more control over the formatting of the error message. Here's an example code using "fprintf":

```C++
#include <stdio.h>

int main()
{
  int num = 5;
  if(num < 10) {
    fprintf(stderr, "Error: Number is less than 10. \n");
  }
  return 0;
}
```

The output of this code will be the same as the previous example. However, in this case, the error message is formatted using the "fprintf" function.

## Deep Dive

While writing to standard error may seem like a simple task, it is important for developers to understand the differences between standard error and standard output (cout) in C++. Standard error should be used specifically for displaying error messages, while standard output is used for general program output. By using standard error, developers can differentiate between regular program output and error messages, making it easier to identify and handle errors.

It is also important to mention that when writing to standard error, the error message will be displayed on the console regardless of whether output is redirected to a file. This can be useful for debugging purposes, as all error messages will still be shown to the developer.

## See Also

Here are some additional resources for writing to standard error in C++:

- [C++ Tutorial: Standard Input and Output](https://www.programiz.com/cpp-programming/input-output)
- [C++ Reference: cerr object](https://www.cplusplus.com/reference/iostream/cerr/)
- [C++ Reference: fprintf function](https://www.cplusplus.com/reference/cstdio/fprintf/)