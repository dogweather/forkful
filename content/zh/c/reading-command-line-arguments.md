---
title:                "C: 读取命令行参数"
simple_title:         "读取命令行参数"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## 为什么

命令行参数是一种非常有用的工具，可以帮助我们在运行程序时提供额外的信息。通过读取命令行参数，我们可以定制程序的行为，使其更加灵活和智能。因此，学习如何读取命令行参数对于任何想要进一步提升其编程技能的人来说都是非常重要的。

## 如何读取命令行参数

在C语言中，读取命令行参数非常简单，我们只需要使用 argc 和 argv 这两个参数即可。下面是一个示例代码，展示了如何读取命令行参数并输出它们：

```c
#include <stdio.h>

// 主函数可以接受两个参数：argc 和 argv
// argc 表示命令行中参数的数量，该参数至少为 1（即程序本身）
// argv 是一个字符串数组，用于存储命令行参数
int main(int argc, char *argv[]) {
    // 使用循环遍历所有的命令行参数，并逐个输出
    for (int i = 0; i < argc; i++) {
        printf("命令行参数 %d 是：%s\n", i, argv[i]);
    }
    return 0;
}
```

假设我们将上面的代码保存为`command_line_args.c`并编译执行，然后在命令行中输入以下命令：

```
./command_line_args hello world
```

那么我们将会得到如下输出：

```
命令行参数 0 是：./command_line_args
命令行参数 1 是：hello
命令行参数 2 是：world
```

通过以上的例子，我们可以看到如何使用 `argc` 和 `argv` 来读取命令行参数，并在程序中进行相应的操作。

## 更深入的了解命令行参数

除了上面提到的使用 `argc` 和 `argv` 读取命令行参数的方法外，我们还可以使用 `getopt()` 函数来解析命令行参数。`getopt()` 函数可以帮助我们处理简单的命令行选项，比如 `-h`、`-v` 等。

另外，我们还可以通过使用 `strtol()`、`strtof()` 或 `strtod()` 等函数来将命令行参数转换为相应的类型，比如整数、浮点数等。

总的来说，读取命令行参数是一个非常灵活和有用的技能，在实际编程中经常会用到。因此，我们应该尽可能多地了解不同的方法和技巧，为我们的程序增加更多的功能和便利性。

## 参考链接

- [C语言教程-命令行参数](https://www.runoob.com/cprogramming/c-command-line-arguments.html)
- [C语言教程-getopt()函数](https://www.runoob.com/cprogramming/c-function-getopt.html)
- [C语言标准库-strtol()函数](https://www.runoob.com/cprogramming/c-function-strtol.html) 

## 参见

- [Markdown 入门](https://www.markdown.xyz/basic-syntax)
- [C语言编程指南](https://www.runoob.com/cprogramming/c-tutorial.html)