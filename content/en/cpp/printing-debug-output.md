---
title:                "C++ recipe: Printing debug output"
programming_language: "C++"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why 

Debugging is an essential part of the software development process. It helps us identify and fix errors and bugs in our code, ensuring our program runs smoothly and as intended. One useful technique for debugging is printing debug output, which allows us to see the values of variables and the flow of our program. This blog post will explore why printing debug output is important and how to do it in C++.

## How To 

To print debug output in C++, we can use the `cout` function from the `iostream` library. Here is an example of how to use it:

```C++
#include <iostream>

using namespace std;

int main() {
  int num1 = 5;
  int num2 = 10;

  cout << "The value of num1 is: " << num1 << endl;
  cout << "The value of num2 is: " << num2 << endl;

  return 0;
}
```

This code will output the following text:

```
The value of num1 is: 5
The value of num2 is: 10
```

We can also use the `cerr` function for error messages and the `clog` function for general program information. Here is an example:

```C++
#include <iostream>

using namespace std;

int main() {
  int num1 = 5;
  int num2 = 0;

  if(num2 == 0) {
    cerr << "Cannot divide by 0!" << endl;
  }

  clog << "Program finished successfully." << endl;

  return 0;
}
```

The `cerr` function will output the error message to the standard error stream and the `clog` function will output the success message to the standard log stream.

## Deep Dive

Printing debug output is helpful in identifying where errors or bugs occur in our code. It allows us to see the values of variables at different points in our program and understand the flow of our program. We can also use it to track the execution of our code and find logical errors that may not be caught by the compiler.

In addition, printing debug output can also be useful for troubleshooting and debugging in larger and more complex programs. It allows us to narrow down the areas where the error may be occurring and make the debugging process more efficient.

However, it is important to note that debugging output should be used only for debugging purposes and should be removed in the final version of our code. Too much debug output can make our program slower and take up unnecessary space in our code.

## See Also 

- [Debugging Techniques in C++](https://www.geeksforgeeks.org/debugging-techniques-in-cpp/)
- [C++ Output Streams](http://www.cplusplus.com/reference/ostream/)
- [Debugging C++ Programs with Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/debugging-cpp-applications-in-visual-studio?view=vs-2019)