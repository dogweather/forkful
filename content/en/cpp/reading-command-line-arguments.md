---
title:                "Reading command line arguments"
html_title:           "C++ recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "C++"
category:             "C++"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Reading command line arguments in C++ refers to the process of passing input data to a program through the command line. This allows programmers to specify different parameters or options when running the program, providing more flexibility and control over its execution.

## How to:
To read command line arguments in C++, we first need to include the `iostream` and `cstring` libraries. Then, we can use the `int main(int argc, char* argv[])` function to access the arguments passed to the program. `argc` stores the number of arguments, while `argv` is a pointer to an array of strings containing the arguments.

```C++
#include <iostream>
#include <cstring>

int main(int argc, char* argv[])
{
  // Check if any arguments were passed
  if (argc > 1) {
    // Loop through each argument
    for (int i = 1; i < argc; i++) {
      // Print the argument and its index
      std::cout << "Argument " << i << ": " << argv[i] << std::endl;
    }
  } 
  else {
    std::cout << "No arguments passed!" << std::endl;
  }
  
  return 0;
}
```
### Sample output (command line input: `program.exe Hello World`):
```
Argument 1: Hello
Argument 2: World
```

## Deep Dive:
Command line arguments have been used in programming languages since the early days of computing. They were originally used to provide input data to programs, as there was no graphical user interface available. Today, they are still commonly used for this purpose, as well as for specifying various options or settings when running a program.

An alternative to reading command line arguments in C++ is to use libraries such as `getopt` or `boost::program_options`. These libraries provide more advanced features for parsing and handling command line arguments, which can be useful for larger and more complex programs.

In terms of implementation, command line arguments are stored as an array of strings, with the first element being the name of the program. The `main` function expects two parameters – `argc` and `argv` – which are automatically passed by the operating system when the program is executed.

## See Also:
- [Command Line Arguments in C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [The getopt function](https://www.gnu.org/software/libc/manual/html_node/Using-Getopt.html)
- [Boost.Program_options library](https://www.boost.org/doc/libs/1_77_0/doc/html/program_options.html)