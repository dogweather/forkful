---
date: 2024-01-20 18:02:51.315083-07:00
description: "How to: When starting, choose your build system or IDE. For simplicity,\
  \ we\u2019ll use a basic text editor and g++. Create two files: `main.cpp` and a\u2026"
lastmod: '2024-03-13T22:45:00.358161-06:00'
model: gpt-4-1106-preview
summary: When starting, choose your build system or IDE.
title: Starting a new project
weight: 1
---

## How to:
When starting, choose your build system or IDE. For simplicity, we’ll use a basic text editor and g++. Create two files: `main.cpp` and a `Makefile`.

`main.cpp`:
```C++
#include <iostream>

int main() {
    std::cout << "Hello, new project!" << std::endl;
    return 0;
}
```

`Makefile`:
```make
all:
    g++ main.cpp -o my_project

clean:
    rm my_project
```

To compile, run `make` in the terminal. To clean up, run `make clean`. 

Sample output after running `./my_project`:
```
Hello, new project!
```

## Deep Dive
Historically, setting up a new C++ project was a more manual process. Today, IDEs can generate templates. Choices like CMake or Meson help manage builds. Before these tools, developers wrote Makefiles by hand, compiling every `.cpp` file into an object file before linking them.

Considering alternatives: newer build systems simplify the process. For example, CMake autogenerates your Makefiles, making it platform-independent.

Implementation-wise, the setup depends on factors like project size and dependencies. Larger projects demand a more complex structure with separate folders for source files, headers, and tests.

## See Also
- [CMake Documentation](https://cmake.org/documentation/)
- [C++ Core Guidelines](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines)
- [GCC, the GNU Compiler Collection](https://gcc.gnu.org/)
