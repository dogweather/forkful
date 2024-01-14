---
title:                "C++ recipe: Writing to standard error"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

Writing to standard error may not be a common practice in everyday programming, but it can be a useful tool for debugging and error handling. By writing to standard error, you can print out specific error messages that can help you identify and fix issues in your code.

## How To

To write to standard error in C++, you can use the `std::cerr` stream. This stream is specifically designed for error output, so any message you write to it will be automatically displayed in the standard error stream.

To use the `std::cerr` stream, you will need to include the `<iostream>` header in your code. Then, you can use the `<<` operator to send messages to the stream, just like you would with `std::cout`.

```C++
#include <iostream>

int main() {
    std::cerr << "This is an error message." << std::endl;
    return 0;
}
```

Running this code will produce the following output in the standard error stream:

```
This is an error message.
```

You can also use the `std::cerr` stream to print out variable values or other important information during debugging.

```C++
#include <iostream>

int main() {
    int num = 42;
    std::cerr << "The value of num is: " << num << std::endl;
    return 0;
}
```

This code will output the following in the standard error stream:

```
The value of num is: 42
```

## Deep Dive

The standard error stream, `std::cerr`, is actually a predefined object of the `std::ostream` class, just like `std::cout`. This means that you can use the same methods and operators on `std::cerr` as you would with `std::cout`.

Additionally, the `<<` operator works differently for `std::cerr` than it does for `std::cout`. With `std::cerr`, the output is immediately flushed to the standard error stream, whereas with `std::cout`, the output is buffered and may not appear until the buffer is full or until you manually flush it.

It's also worth noting that `std::cerr` is unbuffered by default, meaning that its output is not stored in a buffer before being sent to the standard error stream. This ensures that error messages will be immediately displayed, even if your program crashes.

## See Also

- [Printing Strings to standard error using fprintf() in C](https://www.includehelp.com/c-programming/write-to-standard-error-using-fprintf-in-c.aspx)
- [Printing Error Messages to standard error in C++](https://www.tutorialspoint.com/printing-error-messages-to-standard-error-in-cplusplus)
- [The standard streams in C++](https://www.learncpp.com/cpp-tutorial/the-standard-streams/)

By using the `std::cerr` stream in your C++ programs, you can easily print out error messages and important information to the standard error stream. This can be incredibly helpful in debugging and troubleshooting your code. Give it a try in your next project!