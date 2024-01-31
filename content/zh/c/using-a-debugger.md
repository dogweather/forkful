---
title:                "使用调试器"
date:                  2024-01-26T03:48:19.906313-07:00
model:                 gpt-4-0125-preview
simple_title:         "使用调试器"

category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/using-a-debugger.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
调试器是一种工具，它允许您在C代码运行时逐步检查，以追踪错误。程序员使用调试器来了解其代码的行为、修复问题并优化性能，而无需进行猜测。

## 如何操作：
假设您正在处理一个简单的C程序，该程序计算一个数的阶乘，但出现了故障。要使用像`gdb`（GNU Debugger）这样的调试器，请首先使用`-g`标志编译，以包含调试信息：

```c
// 使用以下命令编译: gcc factorial.c -o factorial -g
#include <stdio.h>

long factorial(int n) {
    if (n < 0) return 0; // 一个简单的负数输入检查
    long result = 1;
    while (n > 1)
        result *= n--;
    return result;
}

int main() {
    int number = 5;
    long result = factorial(number);
    printf("The factorial of %d is %ld\n", number, result);
    return 0;
}
```

然后在gdb中运行它：

```shell
$ gdb ./factorial
```

在`factorial`函数处设置断点并运行程序：

```gdb
(gdb) break factorial
(gdb) run
```

当程序达到断点时，使用`next`或`n`逐行步进，并使用`print`或`p`检查变量：

```gdb
(gdb) next
(gdb) print result
$1 = 1
```

样本输出将提供实时值和程序执行流程。

## 深入探讨
调试器自1960年代以来一直存在，从简单的监控器发展到复杂的基于GUI的应用程序。在成熟的调试器开发之前，常见的基于打印的调试是普遍的。`gdb`的替代品包括`lldb`、`dbx`或集成到IDE中的调试器，如Visual Studio或CLion中的那些。

在处理调试器时，实现方式各不相同——有些可以捕获运行时错误，检查内存，甚至反转程序的执行。`gdb`可以附加到正在运行的进程上，允许对已经运行的软件进行调试，这对于修复实时系统错误非常有益。

## 另请参见
- GNU调试器（GDB）：https://www.gnu.org/software/gdb/documentation/
- 使用GDB调试：https://sourceware.org/gdb/current/onlinedocs/gdb
- LLDB调试器：https://lldb.llvm.org/use/tutorial.html
- C中的调试技巧：http://www.cprogramming.com/debugging/debugging.html
