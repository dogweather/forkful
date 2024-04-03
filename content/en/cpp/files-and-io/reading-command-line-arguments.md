---
date: 2024-01-20 17:55:31.779657-07:00
description: "Command line arguments let users influence a program's behavior without\
  \ changing the code. Programs use them to get input params, filepath, or operation\u2026"
lastmod: '2024-03-13T22:45:00.371443-06:00'
model: gpt-4-1106-preview
summary: Command line arguments let users influence a program's behavior without changing
  the code.
title: Reading command line arguments
weight: 23
---

## How to:
In C++, command-line arguments are received in `main()` as an array of character pointers. Here's how you grab them:

```C++
#include <iostream>
int main(int argc, char* argv[]) {
    std::cout << "You've entered " << argc << " arguments:\n";
    for (int i = 0; i < argc; ++i) {
        std::cout << argv[i] << "\n";
    }
    return 0;
}
```

Sample Output: (Assuming executed as `./myProgram foo bar`)

```plaintext
You've entered 3 arguments:
./myProgram
foo
bar
```

## Deep Dive
Way back when, command line was the only way to interact with programs. Today's GUIs are grand, but command line persists, especially in server or development environments. It offers speedy, scriptable control.

Alternatives to built-in `argv` and `argc` include libraries like `Boost.Program_options` for fancier parsing. There's also the `getopt()` function in Unix-like systems for more traditional command line fans.

Implementing argument parsing from scratch lets you tailor it, but watch for security holes. Don't trust user input blindly—always validate and sanitize.

## See Also
- C++ docs on `main()` function: https://en.cppreference.com/w/cpp/language/main_function
- Boost.Program_options: https://www.boost.org/doc/libs/release/libs/program_options/
- GNU `getopt()` tutorial: https://www.gnu.org/software/libc/manual/html_node/Getopt.html
