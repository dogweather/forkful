---
title:                "Starting a new project"
html_title:           "Bash recipe: Starting a new project"
simple_title:         "Starting a new project"
programming_language: "C"
category:             "C"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/starting-a-new-project.md"
---

{{< edit_this_page >}}

# Starting a New C Project: A Programmer's Guide

## What & Why?
Starting a new project is the process of setting up the foundations for fresh code. It's the blank canvas for programmers, allowing them to structure and organize their work as they bring their ideas to life.

## How to:
To start a new project in C, you will need a source file and a makefile to manage the build. Here's a quick example:

1. Your source file (`main.c`):
```C
#include<stdio.h>

int main() {
    printf("Hello, World!\n");
    return 0;
}
```

2. Your makefile:
```C
myProgram: main.c
	gcc -o myProgram main.c
```
3. To compile and run it, open your terminal, navigate to your project directory and enter:
```bash
make myProgram
./myProgram
```
4. The output should be:
```bash
Hello, World!
```

## Deep Dive
Starting a new C project has roots in the early 70s when the language was born in Bell Labs. Setting up your project is generally done manually, but alternatives exist. 

IDEs like Eclipse or CodeBlocks offer automated project setup. Using an Integrated Development Environment can simplify the initial process, arranging necessary files into a predefined structure. However, manual setup provides the flexibility to tailor the project to specific requirements.

When starting a project manually, implementation details vary based on your plan. Factors to consider include project type, whether you'll be using third-party libraries, and how to handle version control. A properly structured project is easier to debug, modify, and maintain.

## See Also
- Basics of makefiles: [GNU Make](https://www.gnu.org/software/make/manual/make.html)
- Info on C language: [C Programming Wiki](https://en.wikibooks.org/wiki/C_Programming)
- IDEs : [Eclipse](https://www.eclipse.org/ide/), [CodeBlocks](http://www.codeblocks.org/)
- More about Version Control Systems: [Git Tutorial](https://git-scm.com/docs/gittutorial)