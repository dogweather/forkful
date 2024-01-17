---
title:                "Printing debug output"
html_title:           "C++ recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Debug output refers to the process of printing information to the console or output window during the execution of a program, for the purpose of troubleshooting and identifying errors. Programmers use this technique to gain insight into the inner workings of their code and to track down the source of bugs.

## How to:

To print debug output in C++, you can use the `cout` function from the `iostream` library. It takes in a string (or any other data type) as a parameter and prints it to the console. For example:

```C++
#include <iostream>

using namespace std;

int main() {
    int num1 = 10;
    int num2 = 20;

    cout << "The value of num1 is: " << num1 << endl;
    cout << "The value of num2 is: " << num2 << endl;

    // Output:
    // The value of num1 is: 10
    // The value of num2 is: 20

    return 0;
}
```

This code snippet will print the values of `num1` and `num2` to the console, allowing the programmer to check if they are correct.

## Deep Dive:

Debug output has been a common technique used by programmers for decades. In the early days of programming, it was the only way to track down errors and bugs. However, as computer systems became more advanced, specialized debugging tools were developed, making the process more efficient and less time-consuming.

One alternative to using `cout` for debug output is to use a debugger tool. Debuggers allow programmers to step through their code line by line and inspect variables and code behavior in real-time. Another option is to use logging libraries, which provide more advanced features such as logging levels and formatting.

When printing debug output, it is important to keep in mind that it can have a performance impact on the program. Therefore, it is recommended to use it sparingly and to remove it once the debugging process is complete.

## See Also:

- [The History of Debugging: A Love Story](https://www.youtube.com/watch?v=YJnF0XIJkr0)
- [An Overview of Debugging Tools for C++](https://raygun.com/blog/debugging-tools-for-cpp/)
- [Introduction to Logging in C++](https://www.codeproject.com/Articles/203887/Introduction-to-Logging-in-Cplusplus)