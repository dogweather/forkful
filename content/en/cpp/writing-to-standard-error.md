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

## Why

As a programmer, you've probably encountered errors in your code that disrupt the smooth execution of your program. These errors are usually displayed on the standard output, but sometimes it can be useful to redirect them to the standard error instead. In this article, we'll explore why you might want to do this and how to write to the standard error in C++.

## How To

To write to the standard error in C++, you will need to include the `iostream` header and use the `cerr` object. Let's look at a simple example:

```C++
#include <iostream>

int main() {
    std::cerr << "Oops, something went wrong!" << std::endl;
    return 0;
}
```

In this code, we are using the `cerr` object to output a string to the standard error using the `<<` operator. Don't forget to include the `endl` manipulator to add a newline after the message.

Running this program will display the following output on the standard error:

```
Oops, something went wrong!
```

You can also use the `<<` operator to output variables or objects to the standard error. For example:

```C++
#include <iostream>

int main() {
    int age = 30;
    std::cerr << "My age is: " << age << std::endl;
    return 0;
}
```

This will output the following on the standard error:

```
My age is: 30
```

## Deep Dive

The standard error, commonly referred to as `stderr`, is a standard output stream that is used to display error messages. Unlike the standard output (`stdout`), the standard error is not buffered, which means it is immediately displayed on the screen without any delay.

You might be wondering why we would want to write to the standard error instead of the standard output. The answer is simple: by writing to the standard error, we can differentiate between regular output and error messages. This can be useful when trying to troubleshoot a program or for logging purposes.

It's important to note that writing to the standard error will only display messages on the screen. If you want to save the messages to a file, you will need to use a different approach.

## See Also

- [Writing to Standard Error in C++](https://www.geeksforgeeks.org/g-fact-33-writes-standard-error-c/)
- [C++ Standard Streams](https://www.tutorialspoint.com/cplusplus/cpp_standard_streams.htm)
- [Difference between Standard Output and Standard Error in C/C++](https://www.geeksforgeeks.org/difference-stdout-stderr/)