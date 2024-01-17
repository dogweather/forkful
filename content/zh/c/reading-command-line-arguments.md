---
title:                "读取命令行参数"
html_title:           "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##什么是命令行参数？为什么程序员要使用它？

命令行参数是指在运行程序时，通过命令行输入的额外信息。它们可以用来改变程序的行为，如指定输入文件或设置特定的选项。程序员使用命令行参数是为了增加程序的灵活性和可配置性。

##如何使用命令行参数：

下面是一个简单的代码示例，展示了如何读取命令行参数并将它们打印出来。

```C
#include <stdio.h>

int main(int argc, char* argv[]){
    for(int i = 1; i < argc; i++){
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```
假设我们将上述代码保存为 `args.c`，然后在命令行中输入 `./args hello world`，运行结果如下：

```
Argument 1: hello
Argument 2: world
```

##深入了解命令行参数：

命令行参数最初用于Unix操作系统，在命令行界面中运行程序时，用户可以通过输入不同的命令行参数来改变程序的行为。而在现代的编程语言中，也都提供了相应的API来读取命令行参数，如 Java 中的 `args[]` 和 C++ 中的 `argc` 和 `argv[]`。

除了读取命令行参数外，程序员也可以通过其他方式来获取用户的输入，如从文件或标准输入中读取。但是命令行参数更为简洁和方便，尤其是对于需要频繁更改的选项和标志。

想要进一步深入了解命令行参数的实现原理和更多用法，可以参考相关资料。

##相关资料：

- [Command-line arguments in C](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [Command-line options in Java](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Command-line arguments in C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)