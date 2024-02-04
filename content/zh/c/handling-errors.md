---
title:                "处理错误"
date:                  2024-02-03T17:58:20.180923-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理错误"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/handling-errors.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 C 语言中处理错误涉及到检测和响应程序执行过程中出现的异常情况。程序员这样做是为了防止程序出现错误、崩溃和不可预测的行为，确保软件在各种情况下都能可靠、高效地工作。

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
