---
title:                "Using an interactive shell (REPL)"
aliases: - /en/cpp/using-an-interactive-shell-repl.md
date:                  2024-01-25T03:39:59.702145-07:00
model:                 gpt-4-1106-preview
simple_title:         "Using an interactive shell (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## What & Why?
A REPL (Read-Eval-Print-Loop) is a simple, interactive programming environment. Programmers use it for real-time language experimentation, quick tasks, or to understand new concepts without the overhead of creating full-blown applications.

## How to:
C++ doesn't come with a built-in REPL, but tools like Cling offer that capability. Here's how to use Cling to calculate the sum of two numbers:

```C++
#include <iostream>

int main() {
    int a = 5;
    int b = 7;
    std::cout << "The sum is: " << a + b << std::endl;
    return 0;
}

// Output:
// The sum is: 12
```

Start Cling and enter the code line by line, observing the output after each command. It's immediate feedback, without compiling.

## Deep Dive
REPLs are common for languages like Python or Lisp, and they've been around since the 1960s. For C++, a compiled language, the concept doesn't fit as naturally, which is why tools like Cling exist—they interpret C++ on the fly. Alternatives include online compilers or small-scale test programs compiled traditionally. Cling is built on top of LLVM and Clang, providing a bridge for C++ to be used in an interpreted fashion.

## See Also
- [Cling](https://root.cern/cling/): An interactive C++ interpreter, built on the top of LLVM and Clang libraries.
- [Jupyter Notebooks](https://jupyter.org/): Offers an interactive shell within a notebook environment, supports C++ through the xeus-cling kernel.
- [LLVM](https://llvm.org/): A collection of modular and reusable compiler and toolchain technologies, which Cling builds upon.
