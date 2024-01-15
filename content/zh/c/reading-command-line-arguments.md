---
title:                "阅读命令行参数"
html_title:           "C: 阅读命令行参数"
simple_title:         "阅读命令行参数"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# 为什么要读取命令行参数

读取命令行参数是一个非常有用的技能，它可以让你的程序变得更加灵活和可配置。通过读取命令行参数，你可以在运行程序时给出不同的选项，从而实现不同的功能。这在开发和调试过程中都非常有帮助。

## 如何读取命令行参数

读取命令行参数的方法很简单，只需要使用 `argc` 和 `argv` 这两个变量就可以了。其中，`argc` 表示命令行参数的数量，`argv` 是一个字符串数组，存储了每个参数的内容。下面是一个例子：

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    printf("共有 %d 个命令行参数\n", argc);
    
    // 遍历输出每个参数的内容
    for (int i = 0; i < argc; i++) {
        printf("参数 %d 的值为：%s\n", i+1, argv[i]);
    }

    return 0;
}
```

假设保存为 `arguments.c`，编译并运行这个程序时，可以通过以下方式给出参数：

```
gcc arguments.c -o arguments
./arguments hello world
```

运行结果如下所示：

```
共有 3 个命令行参数
参数 1 的值为：./arguments
参数 2 的值为：hello
参数 3 的值为：world
```

## 深入探讨

除了上述的方法外，还有一种更加灵活的方式来读取命令行参数，就是使用 `getopt()` 函数。它可以通过设定不同的选项来解析命令行参数，并且支持短选项和长选项。详细的用法可以通过 `man getopt` 来查看。

# 参考资料

- [getopt(3)：处理命令行选项的函数](http://man7.org/linux/man-pages/man3/getopt.3.html)
- [The Linux Command Line: A Complete Introduction](http://www.linuxcommand.org/tlcl.php)