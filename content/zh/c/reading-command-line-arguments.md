---
title:                "C: 读取命令行参数"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

#为什么

在C编程中，命令行参数是一个非常重要的概念。它允许你在运行程序时提供额外的参数，从而让你的程序能够根据不同的需求执行不同的操作。阅读命令行参数可以让你的程序变得更加灵活和实用，因此学习如何读取命令行参数是非常有价值的。

#如何做

要读取命令行参数，我们需要使用`argv`和`argc`这两个变量。`argc`表示命令行中的参数数量，而`argv`是一个指向参数字符串的指针数组。

让我们来看一个简单的例子：

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    // 输出命令行参数的数量
    printf("参数数量：%d\n", argc);

    // 遍历参数打印所有参数
    for (int i = 0; i < argc; i++) {
        printf("第%d个参数： %s\n", i, argv[i]);
    }

    return 0;
}
```

如果我们在命令行输入`./program argument1 argument2 argument3`，程序的输出将会是：

```
参数数量：4
第0个参数：./program
第1个参数：argument1
第2个参数：argument2
第3个参数：argument3
```

通过访问`argv`数组中的不同元素，我们就可以获取对应的命令行参数。

#深入探讨

除了基本的方法之外，读取命令行参数还有一些更高级的技巧。例如，我们可以使用`getopt()`函数来读取参数中的选项（以`-`或`--`开头）。我们也可以使用`atof()`和`atoi()`这样的函数将参数转换为数字类型。

另外，对于复杂的程序，我们可以使用一些第三方库来帮助我们更方便地读取和处理命令行参数，例如GNU getopt库。

总的来说，读取命令行参数并不是一件很复杂的事情，但是如果我们能够充分利用它们，就可以让我们的程序变得更加强大和灵活。

#看看这些

- [C语言命令行参数的详细介绍](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [如何使用C语言读取命令行参数](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)
- [利用C语言处理命令行参数的实用技巧](https://www.bogotobogo.com/cplusplus/command_line_arguments.php)