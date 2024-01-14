---
title:    "C++ recipe: Reading command line arguments"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you're new to C++ programming, you may have come across the term "command line arguments" but are unsure of what it means or why it's important. In this blog post, we'll explore the concept of command line arguments and why it's a crucial skill for any C++ programmer to have.

## How To

Reading command line arguments is a way for your program to receive input from the user when it is being executed. To do this, we use the `argc` and `argv` variables in our `main` function. Here's a simple code example:

```C++
#include <iostream>

int main(int argc, char* argv[]) {
    std::cout << "Number of arguments: " << argc << "\n";
    for (int i = 0; i < argc; i++) {
        std::cout << "Argument " << i + 1 << ": " << argv[i] << "\n";
    }
    return 0;
}
```

Running this program from the command line with the arguments `hello world` will output the following:

```
Number of arguments: 3
Argument 1: ./a.out
Argument 2: hello
Argument 3: world
```

As you can see, the first argument `argv[0]` is always the name of our program, while the rest of the arguments are what the user inputs.

## Deep Dive

Now that we know how to read command line arguments, let's dive deeper into some important details. Firstly, `argc` stands for "argument count" and is an integer that tells us how many arguments were passed to our program. `argv` stands for "argument vector" and is an array of strings containing the actual arguments.

It's important to note that the arguments are always passed to the program as strings. This means that if we want to use them as numbers, we have to convert them using functions like `std::stoi` or `std::stod`.

Another useful feature of command line arguments is the ability to pass in flags, which are special arguments preceded by a `-` or `--`. For example, in the command `./a.out -v`, the `-v` flag could mean "verbose mode" and tell our program to output more information.

And lastly, command line arguments can be very useful when we want to provide options or settings to our program without having to hardcode them in the code itself. This makes our programs more versatile and easier to customize.

## See Also

Here are some additional resources for learning more about command line arguments in C++:

- [C++ Command Line Arguments](https://www.tutorialspoint.com/cplusplus/cpp_command_line_arguments.htm)
- [Using argc and argv in C++](https://www.geeksforgeeks.org/argc-argv-c/)
- [Command Line Arguments in C++](https://www.guru99.com/command-line-arguments-in-c.html)