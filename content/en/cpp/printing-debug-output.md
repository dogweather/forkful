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

## Why
Printing debug output is an important aspect of programming as it helps developers identify and fix any errors or bugs in their code. It allows for easier troubleshooting and ensures the overall functionality and stability of the program.

## How To
To print debug output in C++, you can use the `cout` or `printf` statements. These statements will output the specified message or variable to the console for debugging purposes. For example:

```
#include <iostream>

int main() {
    int num = 5;
    // using cout statement
    std::cout << "The value of num is: " << num << std::endl;

    // using printf statement
    printf("The value of num is: %d\n", num);

    return 0;
}
```

The above code will output: `The value of num is: 5` twice, one using `cout` and the other using `printf`.

## Deep Dive
In order to make debug output more efficient and organized, there are a few techniques you can use.

### Custom Debug Macros
You can create custom debug macros to print out useful information while debugging. For example:

```
#include <iostream>

// custom debug macro
#define DEBUG_MSG(x) { std::cout << "Debug: " << x << std::endl; }

int main() {
    int num = 5;
    // using custom debug macro
    DEBUG_MSG("The value of num is: " << num);

    return 0;
}
```

This will output: `Debug: The value of num is: 5`.

### Conditional Debugging
You can also use conditional statements to control when debug output is printed. For example:

```
#include <iostream>

int main() {
    int num = 5;
    // conditional debug output
    if (num == 5) {
        std::cout << "Debug: The value of num is: " << num << std::endl;
    }

    return 0;
}
```

This will only print the debug message if `num` is equal to 5.

## See Also
- [Debugging in C++](https://docs.microsoft.com/en-us/previous-versions/visualstudio/visual-studio-2010/yt4dkbzs(v=vs.100))
- [Cout vs printf in C++](https://www.geeksforgeeks.org/cout-vs-printf-for-debugging-in-c/)