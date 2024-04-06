---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:58:20.180923-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A C \u8BED\u8A00\u6CA1\u6709\u50CF\u5176\
  \u4ED6\u4E00\u4E9B\u8BED\u8A00\u90A3\u6837\u5185\u7F6E\u5BF9\u5F02\u5E38\u7684\u652F\
  \u6301\u3002\u76F8\u53CD\uFF0C\u5B83\u4F9D\u8D56\u4E8E\u4E00\u4E9B\u4F20\u7EDF\u7684\
  \u9519\u8BEF\u5904\u7406\u7B56\u7565\uFF0C\u5982\u4ECE\u51FD\u6570\u8FD4\u56DE\u7279\
  \u6B8A\u503C\u548C\u8BBE\u7F6E\u5168\u5C40\u53D8\u91CF\u5982 `errno`\u3002 **\u8FD4\
  \u56DE\u7279\u6B8A\u503C** \u51FD\u6570\u53EF\u4EE5\u901A\u8FC7\u8FD4\u56DE\u4E00\
  \u4E2A\u4E0D\u592A\u53EF\u80FD\u662F\u6709\u6548\u7ED3\u679C\u7684\u7279\u5B9A\u503C\
  \u6765\u6307\u793A\u9519\u8BEF\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u5173\u4E8E\u6574\
  \u6570\u7684\u4F8B\u5B50\uFF1A."
lastmod: '2024-04-05T21:53:48.605516-06:00'
model: gpt-4-0125-preview
summary: "**\u8FD4\u56DE\u7279\u6B8A\u503C** \u51FD\u6570\u53EF\u4EE5\u901A\u8FC7\u8FD4\
  \u56DE\u4E00\u4E2A\u4E0D\u592A\u53EF\u80FD\u662F\u6709\u6548\u7ED3\u679C\u7684\u7279\
  \u5B9A\u503C\u6765\u6307\u793A\u9519\u8BEF\u3002\u8FD9\u91CC\u6709\u4E00\u4E2A\u5173\
  \u4E8E\u6574\u6570\u7684\u4F8B\u5B50\uFF1A."
title: "\u5904\u7406\u9519\u8BEF"
weight: 16
---

## 如何操作：
C 语言没有像其他一些语言那样内置对异常的支持。相反，它依赖于一些传统的错误处理策略，如从函数返回特殊值和设置全局变量如 `errno`。

**返回特殊值**

函数可以通过返回一个不太可能是有效结果的特定值来指示错误。这里有一个关于整数的例子：

```c
#include <stdio.h>

int inverse(int number, double *result) {
    if (number == 0) {
        return -1; // 错误情况
    } else {
        *result = 1.0 / number;
        return 0; // 成功
    }
}

int main() {
    double result;
    if (inverse(0, &result) < 0) {
        printf("错误：除以零。\n");
    } else {
        printf("倒数是：%f\n", result);
    }
    
    return 0;
}
```

**输出：**
```
错误：除以零。
```

**检查 `errno`**

对于库函数，特别是那些与系统或操作系统（如文件 I/O）交互的函数，当发生错误时会设置 `errno`。要使用它，需要引入 `errno.h` 并在怀疑失败后检查 `errno`：

```c
#include <stdio.h>
#include <errno.h>
#include <string.h>

int main() {
    FILE *file = fopen("nonexistent.txt", "r");
    if (file == NULL) {
        printf("打开文件错误：%s\n", strerror(errno));
    }
    
    return 0;
}
```

**输出：**
```
打开文件错误：没有那个文件或目录
```

## 深入了解
历史上，C 编程语言的简约设计排除了内置的异常处理机制，反映了其底层系统编程的起源，其中最大的性能和接近金属层的控制至关重要。相反，C 采用了一种更手工的错误处理方法，符合其尽可能给程序员提供最大控制权的理念，即使这样做牺牲了便利性。

虽然这种方法与 C 的设计目标非常吻合，但它也可能导致冗长的错误检查代码和可能遗漏的错误检查，而现代语言则通过结构化异常处理机制解决了这个问题。例如，像 Java 或 C# 这样的语言中的异常允许进行集中错误处理，使代码更加清晰，错误管理也更加直接。然而，异常引入了它们的开销和复杂性，这可能不适合 C 所擅长的系统级编程。

尽管它的粗糙，C 中的手动错误处理已经影响了许多其他语言的错误管理设计，提供了一个模型，其中错误条件的明确性可以导致更可预测和可调试的代码。对于必须优雅管理失败的关键系统而言，C 的错误处理范式——结合现代最佳实践，如错误处理库和约定——确保了健壮性和可靠性。
