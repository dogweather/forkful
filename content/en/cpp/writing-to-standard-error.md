---
title:                "Writing to standard error"
html_title:           "C++ recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
In C++, writing to standard error is a way to send error messages or debugging information to the standard error stream, also known as stderr. This allows for a clear separation between regular program output and any errors or warnings that may occur during runtime.

Programmers do this because it helps with troubleshooting and identifying issues in their code. By printing error messages directly to the standard error stream, they can easily distinguish between normal program output and any potential errors.

## How to:
To write to standard error in C++, you can use the `std::cerr` stream. This is a predefined object that represents the standard error stream. To use it, you simply insert the message you want to print into the stream, just like you would with the `std::cout` stream for regular output.

```
#include <iostream>

int main() {
  // write to standard error using std::cerr
  std::cerr << "This is an error message" << std::endl;
  
  // other code
  
  return 0;
}
```

Output:
```
This is an error message
```

You can also use `std::fprintf` to write to standard error, which allows for more control over the formatting of the message.

```
#include <cstdio>

int main() {
  // write to standard error using std::fprintf
  std::fprintf(stderr, "This is an error message: %d\n", 42);
  
  // other code
  
  return 0;
}
```

Output:
```
This is an error message: 42
```

## Deep Dive:
Writing to standard error is a feature that has been around since the early days of C and UNIX. It allows for a clear separation between normal program output and any errors or warnings that may occur. This can be especially helpful when running a program in a command-line environment, where regular output and error messages may be displayed on different streams.

An alternative to writing to standard error is using a log file. This can be useful for keeping a record of all program output, including error messages. However, writing to standard error is generally preferred for troubleshooting and debugging purposes, as it provides real-time feedback during program execution.

In terms of implementation, writing to standard error is typically done by redirecting the standard error stream to the desired output destination, such as a console or log file. This can be done using the command-line `2>` syntax, or within the code using `std::freopen`.

## See Also:
- [C++ I/O Streams](https://www.tutorialspoint.com/cplusplus/cpp_streams.htm)
- [C++ Standard Error Stream (std::cerr)](https://www.geeksforgeeks.org/c-standard-error-stream-cerr/)
- [Redirecting standard error in C++](https://stackoverflow.com/questions/1374804/redirecting-standard-error-in-c-c)