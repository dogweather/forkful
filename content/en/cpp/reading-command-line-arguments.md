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

# Deciphering the Command Line in C++ 

## What & Why?
Reading command line arguments is about fetching user-defined inputs when launching a program from a terminal. Programmers do it to make their programs more flexible and user-responsive.

## How to:
A glimpse of how to play around with command line arguments in C++. 

```C++
#include <iostream>

int main(int argc, char *argv[]) {
    // argc gives the count of arguments, argv stores those arguments.
    for(int i = 1; i < argc; ++i) {
        std::cout << "Argument " << i << " : " << argv[i] << '\n';
    }
    return 0;
}
```

If you fire up the terminal and run, say, `./program firstArg secondArg`, your output will be:

```C++
Argument 1 : firstArg
Argument 2 : secondArg
```

## Deep Dive 
Historically, we've used the command line to interact with our machines. Even in this age of GUIs, command line arguments help us automate tasks and manage systems. `argc` and `argv` are passed to `main()` to record what the user enters on the command line when invoking the program. 

But there are alternatives. Libraries like `getopt` and Boost's Program Options offer a higher, more abstract interface for command line parsing. They provide easy methods for handling flags, options, and usage descriptions.

As for the nitty-gritty, C++ conventionally executes the `main()` function first. If invoked from the command line, `argc` gives the total count of command line arguments and `argv` is an array of these arguments. `argv[0]` points to the program name itself, `argv[1]` points to the first command line argument, and so forth.

## See Also
To increase your command line kung fu, check out:
- getopt - https://www.gnu.org/software/libc/manual/html_node/Getopt.html
- Boost Program Options: - https://www.boost.org/doc/libs/1_75_0/doc/html/program_options.html
- Effective C++: 55 Specific Ways to Improve Your Programs and Designs - https://www.oreilly.com/library/view/effective-c/9780321334879/