---
title:                "C++ recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error is an important aspect of debugging and error handling in C++ programming. It allows developers to easily identify and handle errors in their code, making it an essential skill for any C++ programmer.

## How To

To write to standard error in C++, you can use the `std::cerr` stream. This stream is specifically designed for printing error messages to the standard error output.

```C++
#include <iostream>

int main() {
  std::string message = "A sample error message.";
  std::cerr << message << std::endl;

  return 0;
}
```

The above code will output "A sample error message." to the standard error output. Note that using `std::endl` is important as it adds a new line character to the end of the output.

You can also use the `fprintf()` function from the C standard library to write to standard error. However, this method is more verbose and not as robust as using the `std::cerr` stream.

```C++
#include <cstdio>

int main() {
  std::string message = "Another sample error message.";
  fprintf(stderr, "%s\n", message.c_str()); // note the use of .c_str() to convert the string to a C-style string.

  return 0;
}
```

## Deep Dive

Writing to standard error is often used in conjunction with exception handling. When an exception is thrown, the `std::cerr` stream is typically used to print out the details of the error, allowing developers to easily identify the source of the problem.

It is also important to note that standard error output is different from standard output, which is represented by the `std::cout` stream. Standard output is used for regular program output, while standard error is reserved for error messages and debugging information.

## See Also

- [C++ Standard Library Reference](https://en.cppreference.com/w/cpp/header)
- [Exception Handling in C++](https://www.geeksforgeeks.org/exception-handling-c/)
- [Difference between Standard Output and Standard Error](https://unix.stackexchange.com/questions/217100/differentiate-between-stdout-and-stderr)

In conclusion, writing to standard error is an essential skill for any C++ programmer. It allows for efficient debugging and error handling, making the development process smoother and more manageable. With a clear understanding of how to write to standard error and its importance in exception handling, you can improve the quality of your code and enhance your overall programming skills.