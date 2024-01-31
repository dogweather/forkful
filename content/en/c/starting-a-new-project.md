---
title:                "Starting a new project"
date:                  2024-01-20T18:02:46.581279-07:00
model:                 gpt-4-1106-preview
simple_title:         "Starting a new project"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why?
Starting a new project in C means setting up a basic structure for your app. Programmers do this to lay a foundation, making sure everything that follows has a tidy place to live.

## How to:
```C
#include <stdio.h>

int main() {
    printf("Hello, new project!\n");
    return 0;
}
```

Run it, and you should see:
```
Hello, new project!
```

## Deep Dive
Back in the 1970s, C was born. Dennis Ritchie started something big at Bell Labs. C's simplicity makes it a go-to even now for system software, embedded systems, and high-performance applications.

When starting out, choose between procedural or modular styles. Procedural is straightforward, akin to following a recipe. Modular lets you organize code in chunks – think ingredients sorted into bowls. Both work, but modular scales better for complex projects.

Under the hood, when you compile, your setup is turned into an executable. The compiler (like GCC) reads your `main()` function as the entry point. But there's more: linking libraries, setting up makefiles for bigger projects, and maybe sprinkling in some preprocessor directives for flavor.

## See Also
- [GNU GCC Compiler](https://gcc.gnu.org/)
- [Makefile Tutorial](https://makefiletutorial.com/)
