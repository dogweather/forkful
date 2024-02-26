---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:29.495111-07:00
description: "\u5728C\u8BED\u8A00\u4E2D\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\
  \u5199\u6D89\u53CA\u5C06\u7ED9\u5B9A\u5B57\u7B26\u4E32\u4E2D\u7684\u6240\u6709\u5927\
  \u5199\u5B57\u6BCD\u8F6C\u6362\u4E3A\u5B83\u4EEC\u76F8\u5E94\u7684\u5C0F\u5199\u7B49\
  \u4EF7\u7269\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u6B64\u64CD\u4F5C\u6765\
  \u6807\u51C6\u5316\u6587\u672C\u8F93\u5165\uFF0C\u4EE5\u4FBF\u8FDB\u884C\u6BD4\u8F83\
  \u3001\u641C\u7D22\u64CD\u4F5C\uFF0C\u6216\u4EC5\u4EC5\u4E3A\u4E86\u8F93\u51FA\u7684\
  \u7F8E\u89C2\u4E00\u81F4\u6027\u3002"
lastmod: '2024-02-25T18:49:45.858652-07:00'
model: gpt-4-0125-preview
summary: "\u5728C\u8BED\u8A00\u4E2D\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\
  \u5199\u6D89\u53CA\u5C06\u7ED9\u5B9A\u5B57\u7B26\u4E32\u4E2D\u7684\u6240\u6709\u5927\
  \u5199\u5B57\u6BCD\u8F6C\u6362\u4E3A\u5B83\u4EEC\u76F8\u5E94\u7684\u5C0F\u5199\u7B49\
  \u4EF7\u7269\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u6267\u884C\u6B64\u64CD\u4F5C\u6765\
  \u6807\u51C6\u5316\u6587\u672C\u8F93\u5165\uFF0C\u4EE5\u4FBF\u8FDB\u884C\u6BD4\u8F83\
  \u3001\u641C\u7D22\u64CD\u4F5C\uFF0C\u6216\u4EC5\u4EC5\u4E3A\u4E86\u8F93\u51FA\u7684\
  \u7F8E\u89C2\u4E00\u81F4\u6027\u3002"
title: "\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A\u5C0F\u5199"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在C语言中将字符串转换为小写涉及将给定字符串中的所有大写字母转换为它们相应的小写等价物。程序员经常执行此操作来标准化文本输入，以便进行比较、搜索操作，或仅仅为了输出的美观一致性。

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
