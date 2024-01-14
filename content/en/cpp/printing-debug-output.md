---
title:    "C++ recipe: Printing debug output"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

When developing a program, it is crucial to have a way to identify and fix any errors or bugs that may arise. This is where printing debug output becomes essential. By displaying specific information during the execution of a program, developers can effectively track down and fix issues.

## How To

To print debug output in a C++ program, we can make use of the `cout` function from the `iostream` library. This function allows us to print text or variables to the console.

Let's look at an example:

```C++
#include <iostream>

using namespace std;

int main() {
    int num1 = 10;
    int num2 = 5;
    cout << "The value of num1 is: " << num1 << endl;
    cout << "The value of num2 is: " << num2 << endl;
    return 0;
}
```

In this example, we have two variables, `num1` and `num2`, and we use the `cout` function to print their values to the console. The `<<` operator is used to concatenate the text and variables, and `endl` is used to insert a line break.

The output of this program will be:

```
The value of num1 is: 10
The value of num2 is: 5
```

By printing relevant information, such as variable values or the current state of a loop, we can track the flow of our program and identify any errors or unexpected behavior.

## Deep Dive

When using the `cout` function, we can also format the output by using flags such as `setw` and `setprecision`. This allows us to control the spacing and precision of floating-point numbers, making our debug output more organized and informative.

We can also use the `cerr` function for error output instead of `cout`. Unlike `cout`, `cerr` does not buffer its output, which means we can receive error messages in real-time, even in the case of a program crash.

Lastly, we can use conditional statements such as `#ifdef` to print debug output only during development and not in the final production version of our program. This saves us from the overhead of printing unnecessary information and makes our code more efficient.

## See Also

- [C++ Debugging Techniques](https://www.pluralsight.com/blog/software-development/c-plus-plus-debugging-techniques)
- [Debugging with Visual Studio Code](https://code.visualstudio.com/docs/cpp/debugging)
- [Debugging C++ Programs using GDB](https://www.thegeekstuff.com/2010/03/debug-c-program-using-gdb)

With the use of printing debug output, we can effectively debug our C++ programs and improve their functionality. Make sure to give it a try in your next project and see its benefits for yourself! Happy coding!