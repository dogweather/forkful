---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:09:59.373394-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A GDB\uFF08GNU\u8C03\u8BD5\u5668\uFF09\u662F\
  C\u7F16\u7A0B\u6700\u5E38\u7528\u7684\u8C03\u8BD5\u5668\u3002\u4EE5\u4E0B\u662F\u4F7F\
  \u7528GDB\u8C03\u8BD5\u4E00\u4E2A\u7B80\u5355C\u7A0B\u5E8F\u7684\u7B80\u8981\u6307\
  \u5357\u3002 \u9996\u5148\uFF0C\u4F7F\u7528`-g`\u6807\u5FD7\u7F16\u8BD1\u60A8\u7684\
  C\u7A0B\u5E8F\uFF0C\u4EE5\u5305\u542B\u8C03\u8BD5\u4FE1\u606F\uFF1A."
lastmod: '2024-04-05T22:38:47.463232-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A GDB\uFF08GNU\u8C03\u8BD5\u5668\uFF09\u662F\
  C\u7F16\u7A0B\u6700\u5E38\u7528\u7684\u8C03\u8BD5\u5668\u3002\u4EE5\u4E0B\u662F\u4F7F\
  \u7528GDB\u8C03\u8BD5\u4E00\u4E2A\u7B80\u5355C\u7A0B\u5E8F\u7684\u7B80\u8981\u6307\
  \u5357\u3002 \u9996\u5148\uFF0C\u4F7F\u7528`-g`\u6807\u5FD7\u7F16\u8BD1\u60A8\u7684\
  C\u7A0B\u5E8F\uFF0C\u4EE5\u5305\u542B\u8C03\u8BD5\u4FE1\u606F\uFF1A."
title: "\u4F7F\u7528\u8C03\u8BD5\u5668"
weight: 35
---

## 如何操作：
GDB（GNU调试器）是C编程最常用的调试器。以下是使用GDB调试一个简单C程序的简要指南。

首先，使用`-g`标志编译您的C程序，以包含调试信息：

```c
gcc -g program.c -o program
```

接下来，用您编译好的程序启动GDB：

```bash
gdb ./program
```

现在，您可以使用GDB内的各种命令来控制其操作。以下是一些基本命令：

- `break`：在指定的行或函数上设置断点，以暂停执行。
  - 示例：`break 10` 或 `break main`
- `run`：在GDB内开始执行您的程序。
- `next`：执行下一行代码，而不进入函数内部。
- `step`：执行下一行代码，进入函数内部。
- `print`：显示变量的值。
- `continue`：继续执行，直到下一个断点。
- `quit`：退出GDB。

以下是调试一个简单程序的示例会话：

```c
#include <stdio.h>

int main() {
    int i;
    for (i = 0; i < 5; i++) {
        printf("%d\n", i);
    }
    return 0;
}
```

按照描述编译并启动GDB。用`break 5`设置一个在`printf`行的断点，然后`run`。使用`next`来逐步执行循环，并用`print i`来检查循环变量。

在设置断点并在第一次迭代之前的示例输出：

```
Breakpoint 1, main () at program.c:5
5         printf("%d\n", i);
```

经过几次迭代后使用`print i`：

```
$3 = 2
```

这展示了检查一个简单程序的状态和流程。

## 深入探究
自编程早期以来，调试的概念已经发生了显著的进化，那时物理缺陷（字面上的昆虫）可能在机械计算机中造成问题。如今，GDB等调试器提供了超出基本步骤和变量检查的复杂功能，如逆向调试（向后执行程序）、条件断点和为自动调试任务编写脚本。

虽然GDB功能强大且广泛使用，但对初学者来说可能复杂且具挑战性。像Visual Studio Code、CLion或Eclipse这样的替代调试工具和IDE（集成开发环境）为调试C代码提供了更友好的界面，通常整合了视觉辅助和更直观的控制。这些替代方案可能不提供GDB全部的功能深度，但对C编程的新手来说可能更易于使用。

此外，语言服务器协议和调试标准的出现促进了跨平台调试解决方案，使不同工具和环境中的调试体验更加一致。尽管有了这些进步，但学习像GDB这样的传统调试器的来龙去脉，对于理解C程序的执行提供了宝贵的见解，并且仍然是开发人员工具箱中的一项关键技能。
