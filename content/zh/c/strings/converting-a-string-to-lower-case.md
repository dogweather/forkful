---
title:                "将字符串转换为小写"
aliases:
- /zh/c/converting-a-string-to-lower-case.md
date:                  2024-02-03T17:54:29.495111-07:00
model:                 gpt-4-0125-preview
simple_title:         "将字符串转换为小写"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-string-to-lower-case.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
