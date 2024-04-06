---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:03.643262-07:00
description: "\u5982\u4F55\u64CD\u4F5C: \u5728C\u8BED\u8A00\u4E2D\uFF0C\u5B57\u7B26\
  \u4E32\u662F\u4EE5\u7A7A\u5B57\u7B26\uFF08`\\0`\uFF09\u7ED3\u5C3E\u7684\u5B57\u7B26\
  \u6570\u7EC4\u3002\u4E0D\u50CF\u5728\u66F4\u9AD8\u7EA7\u7684\u8BED\u8A00\u4E2D\uFF0C\
  C\u8BED\u8A00\u6CA1\u6709\u63D0\u4F9B\u5185\u7F6E\u7684\u5B57\u7B26\u4E32\u8FDE\u63A5\
  \u51FD\u6570\u3002\u76F8\u53CD\uFF0C\u4F60\u9700\u8981\u4F7F\u7528 `<string.h>`\
  \ \u5E93\u4E2D\u7684 `strcat()` \u6216 `strncat()` \u51FD\u6570\u3002 \u8FD9\u91CC\
  \u6709\u4E00\u4E2A\u4F7F\u7528 `strcat()` \u7684\u7B80\u5355\u4F8B\u5B50\uFF1A."
lastmod: '2024-04-05T22:38:47.445615-06:00'
model: gpt-4-0125-preview
summary: "\u5728C\u8BED\u8A00\u4E2D\uFF0C\u5B57\u7B26\u4E32\u662F\u4EE5\u7A7A\u5B57\
  \u7B26\uFF08`\\0`\uFF09\u7ED3\u5C3E\u7684\u5B57\u7B26\u6570\u7EC4\u3002\u4E0D\u50CF\
  \u5728\u66F4\u9AD8\u7EA7\u7684\u8BED\u8A00\u4E2D\uFF0CC\u8BED\u8A00\u6CA1\u6709\u63D0\
  \u4F9B\u5185\u7F6E\u7684\u5B57\u7B26\u4E32\u8FDE\u63A5\u51FD\u6570\u3002\u76F8\u53CD\
  \uFF0C\u4F60\u9700\u8981\u4F7F\u7528 `<string.h>` \u5E93\u4E2D\u7684 `strcat()`\
  \ \u6216 `strncat()` \u51FD\u6570\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

## 如何操作:
在C语言中，字符串是以空字符（`\0`）结尾的字符数组。不像在更高级的语言中，C语言没有提供内置的字符串连接函数。相反，你需要使用 `<string.h>` 库中的 `strcat()` 或 `strncat()` 函数。

这里有一个使用 `strcat()` 的简单例子：

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";

    strcat(destination, source);

    printf("%s\n", destination);  // 输出：Hello, World!
    return 0;
}
```

`strcat()` 函数接受两个参数：目标字符串（必须有足够的空间来保存连接的结果）和源字符串。然后，它将源字符串追加到目标字符串上。

为了更多地控制连接的字符数，使用 `strncat()` 更安全：

```c
#include <stdio.h>
#include <string.h>

int main() {
    char destination[50] = "Hello, ";
    char source[] = "World!";
    int num = 3; // 要追加的字符数

    strncat(destination, source, num);

    printf("%s\n", destination);  // 输出：Hello, Wor
    return 0;
}
```

这限制了将源字符串的前 `num` 个字符连接起来，有助于防止缓冲区溢出。

## 深入探索
函数 `strcat()` 和 `strncat()` 自 C 标准库诞生以来就一直是其一部分，反映了该语言需要手动管理字符串和内存的低级特性。与许多将字符串视为带有内置连接运算符（如 `+` 或 `.concat()`）的一等对象的现代编程语言不同，C 语言的做法需要更深入的理解指针、内存分配以及像缓冲区溢出这样的潜在陷阱。

尽管 `strcat()` 和 `strncat()` 被广泛使用，但如果使用不当，它们可能会创建安全漏洞而受到批评。数据超出分配的内存可以导致崩溃或被用于任意代码执行的缓冲区溢出，是一种潜在风险。因此，程序员越来越多地转向使用像 `snprintf()` 这样更安全的替代品，它通过限制基于目标字符串大小而写入目标字符串的字符数来提供更可预测的行为：

```c
char destination[50] = "Hello, ";
char source[] = "World!";
snprintf(destination + strlen(destination), sizeof(destination) - strlen(destination), "%s", source);
```

这种方法虽然更冗长，但显著更安全，凸显了 C 编程实践朝着优先考虑安全性和稳健性而不是简洁性的转变。

尽管存在这些挑战，C语言中的字符串连接是一项基础技能，对于该语言的有效编程至关重要。理解其细微差别和相关风险是掌握 C 编程的关键。
