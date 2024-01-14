---
title:    "C++ recipe: Reading command line arguments"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why
When writing a program in C++, it's sometimes necessary to provide input through the command line instead of hardcoded values. This allows for more dynamic and customizable programs, making it a useful skill for any programmer to learn.

## How To
Reading command line arguments is simple and straightforward in C++. We start by declaring the main() function and including the <iostream> and <string> libraries. Then, within the main() function, we use the argc and argv parameters to access the command line arguments.

```C++
#include <iostream>
#include <string>

using namespace std;

int main(int argc, char* argv[])
{
    // argc stores the number of command line arguments
    // argv is an array of strings that holds the actual arguments

    // To print out all the arguments entered by the user
    for (int i = 0; i < argc; i++)
    {
        cout << argv[i] << endl;
    }

    // To access a specific argument
    string input = argv[1]; // index starts at 0, so argv[1] is the second argument

    return 0;
}
```

### Sample Output
If we run the above program with the following command line arguments: "my_program.exe Hello World 123", the output would be:
```
my_program.exe
Hello
World
123
```

## Deep Dive
There are a few key things to keep in mind when reading command line arguments in C++.

1. The first argument in argv, argv[0], is always the name of the program itself.
2. The arguments entered by the user are separated by spaces.
3. If the argument contains spaces, it must be surrounded by quotation marks.
4. Every argument is stored as a string, so if we want to perform any numeric operations, we must first convert the string to the desired data type.

## See Also
- [C++ Command Line Arguments Tutorial](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Official Documentation on Command Line Arguments](https://docs.microsoft.com/en-us/cpp/cpp/main-function-command-line-args?view=vs-2019)