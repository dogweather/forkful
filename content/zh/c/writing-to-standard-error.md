---
title:                "标准错误写作"
html_title:           "C: 标准错误写作"
simple_title:         "标准错误写作"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# 为什么

为什么要把信息写入到标准错误（standard error）中？当程序发生错误时，标准错误是一个非常有用的工具，它可以帮助我们定位错误的源头并进行调试。因此，学习如何正确地写入到标准错误是一项重要的编码技巧。

# 如何做

要把信息写入到标准错误中，我们需要使用 `stderr` 函数，并且在需要写入错误信息的地方添加 `fprintf(stderr, "错误信息");` 的代码。下面是一个示例：

```
#include <stdio.h>

int main() {
    // 假设这里发生了一个错误
    fprintf(stderr, "发生了一个错误，请检查代码！");
    
    return 0;
}
```

运行程序后，你将在控制台中看到错误信息被打印出来。

# 深入了解

除了用于调试，写入标准错误还有其他用途。比如，当我们编写一个命令行程序时，可以使用标准错误来打印出程序的用法或帮助信息。此外，我们还可以使用 `stderr` 函数来记录程序的运行日志，以便后续分析和排错。

# 更多学习资源

- [C 标准库文档](https://zh.cppreference.com/w/c)
- [如何在 C 语言中调试程序](https://www.runoob.com/w3cnote/c-debugging-tutorial.html)
- [标准错误（stderr）的用法](https://www.geeksforgeeks.org/stderr-in-c-and-its-libraries/)