---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output refers to the act of displaying variable states, flow processes, and functional results to understand program behavior. Programmers use it to identify, trace, and fix bugs or errors in their code by better comprehending its runtime flow.

## How to:

C++ has several methods for printing debug output, most commonly using `cout` statement from the Standard I/O library, or using `cerr` for error messages. Here's an example:

```C++
#include <iostream>

int main() {
    int x = 10;
    std::cout << "Debug: X is " << x << std::endl; // prints: Debug: X is 10
    std::cerr << "An error occurred." << std::endl; // error message
    return 0;
}
```

You can also use the Debug and Trace facilities of the `cassert` library:

```C++
#include <cassert>

int main() {
    int x = 5, y = 0;
    y = x - 5;
    assert(y != 0 && "Y is zero!"); // halts if y is zero
    return 0;
}
```

## Deep Dive:

Historically, programmers have always needed a means to peek under the hood. Debug output dates back to assembly language, using print statements to trace variable values. 

Alternative methods include logging into files, graphical debuggers, and specialized debug libraries. One such library is `Boost.Log`, designed for logging in multithreaded, heavy-load applications.

The actual implementation of `cout` and `cerr` is intricate. In short, they are stream objects tied to the console's stdout/stderr. Any data sent to these streams is displayed in the console, making them apt for debug output.

## See Also:

- `std::cout` and `std::cerr` reference: [http://www.cplusplus.com/reference/iostream/](http://www.cplusplus.com/reference/iostream/)
- `Boost.Log` library: [https://boost.org/libs/log/](https://boost.org/libs/log/)
- For information on C++ graphical debuggers: [https://www.gnu.org/software/gdb/](https://www.gnu.org/software/gdb/)