---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:29.495111-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4E0E\u4E00\u4E9B\u9AD8\u7EA7\u8BED\u8A00\
  \u4E0D\u540C\uFF0CC\u8BED\u8A00\u6CA1\u6709\u76F4\u63A5\u5C06\u5B57\u7B26\u4E32\u8F6C\
  \u6362\u4E3A\u5C0F\u5199\u7684\u5185\u7F6E\u51FD\u6570\u3002\u7136\u800C\uFF0C\u4F7F\
  \u7528C\u6807\u51C6\u5E93\u51FD\u6570\u53EF\u4EE5\u8F7B\u677E\u5B9E\u73B0\u8FD9\u4E2A\
  \u8FC7\u7A0B\u3002\u4E0B\u9762\u662F\u4E00\u4E2A\u5206\u6B65\u6307\u5357\u548C\u4E00\
  \u4E2A\u793A\u4F8B\uFF0C\u6F14\u793A\u5982\u4F55\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\
  \u4E3A\u5C0F\u5199\u3002"
lastmod: '2024-04-05T21:53:48.576499-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
weight: 4
---

## 如何操作：
与一些高级语言不同，C语言没有直接将字符串转换为小写的内置函数。然而，使用C标准库函数可以轻松实现这个过程。下面是一个分步指南和一个示例，演示如何将字符串转换为小写。

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("Original: %s\n", text);

    toLowerCase(text);
    printf("Lowercase: %s\n", text);

    return 0;
}
```

**示例输出：**

```
Original: Hello, World!
Lowercase: hello, world!
```

在这个示例中，`toLowerCase`函数遍历输入字符串的每个字符，使用`ctype.h`中的`tolower`函数将其转换为小写等价物。修改是就地完成的，更改了原始字符串。

## 深入探讨
上面示例中使用的`tolower`函数是C标准库的一部分，特别在于`ctype.h`头文件中。它基于当前区域设置操作，但对于标准的"C"区域设置，它处理ASCII字符集，其中'A'到'Z'被转换为'a'到'z'。

历史上，C语言中的字符编码和大小写转换与ASCII字符集紧密相关，这限制了它在国际化或本地化应用中的实用性，其中常见的是ASCII集之外的字符。现代编程语言可能提供内置的字符串方法来执行大小写转换，考虑到区域设置和Unicode字符，这是C语言本身所缺乏的。

在需要大量文本操作的场景中，特别是涉及非ASCII字符时，程序员可能会考虑使用提供更好国际化支持的库，如ICU（International Components for Unicode）。然而，对于大多数处理ASCII文本的应用，所展示的方法是高效且简单的。它突出了C语言让程序员控制数据操作的倾向，尽管与高级语言相比，涉及的工作量更大。
