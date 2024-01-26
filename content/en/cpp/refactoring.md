---
title:                "Refactoring"
date:                  2024-01-25T02:12:11.086953-07:00
model:                 gpt-4-1106-preview
simple_title:         "Refactoring"
programming_language: "C++"
category:             "C++"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/refactoring.md"
---

{{< edit_this_page >}}

## What & Why?

Refactoring is the process of changing a computer program's internal structure without altering its external behavior. Programmers do it to clean up their code, making it easier to understand, maintain, and extend.

## How to:

Imagine you've got a function that's doing a bit too much, like this clunky method that initializes an object and also performs logging:

```C++
#include <iostream>

class Widget {
public:
    void init(bool verbose) {
        // Initialization logic
        // ...

        // Verbose logging
        if (verbose) {
            std::cout << "Widget initialized!" << std::endl;
        }
    }
};

// Usage:
Widget w;
w.init(true);
```

Output:
```
Widget initialized!
```

Refactoring this into cleaner, more focused methods might look like this:

```C++
#include <iostream>

class Widget {
public:
    void init() {
        // Initialization logic only
        // ...
    }

    void logInitialization() const {
        std::cout << "Widget initialized!" << std::endl;
    }
};

// Usage:
Widget w;
w.init();
w.logInitialization();
```

This change hasn't altered what the program does but makes the `Widget` class more modular and its usage more clear.

## Deep Dive

The concept of refactoring as we know it today has its roots in the Smalltalk programming communities of the 1980s and was heavily popularized by Martin Fowler's book "Refactoring: Improving the Design of Existing Code" from 1999. Today, refactoring is a core part of modern software development, integrated into various development methodologies such as Agile and TDD (Test-Driven Development).

When we talk about alternatives to refactoring, we veer into the territory of rewriting or redesigning. Refactoring is strategic and incremental, whereas a rewrite may scrap existing code in favor of a new solution. Redesign, meanwhile, may entail more significant changes including altering functionality, which is a non-goal for pure refactoring.

Implementation details on refactoring can get quite granular. There are many 'code smells' that might prompt a refactor, such as long methods, large classes, or duplicated code. Automated tools exist that can assist in refactoring, such as "Clang-Tidy" for C++, which can spot issues and even apply some fixes.

Moreover, refactoring requires a solid suite of tests to ensure that functionality remains unchanged. Without tests, you're essentially flying blind and risking regressions.

## See Also

For a deeper understanding of refactoring and to see more examples, you might want to check out:

- Martin Fowler's classic text "Refactoring: Improving the Design of Existing Code" for foundational ideas and strategies.
- The `Clang-Tidy` documentation at https://clang.llvm.org/extra/clang-tidy/ for automated refactoring support in C++.
- "Working Effectively with Legacy Code" by Michael Feathers, which provides techniques for safely refactoring in the context of less-than-perfect existing codebases.
