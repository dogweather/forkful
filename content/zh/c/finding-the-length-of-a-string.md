---
title:                "C: 查找字符串的长度。"
simple_title:         "查找字符串的长度。"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

**为什么要求字符串的长度？**

字符串是C语言中最常用的数据类型之一，它可以用来存储文本或者字符序列。有时候我们需要知道一个字符串的长度，比如在做字符串比较或者拼接时。因此，找到字符串的长度是非常重要的，让我们来学习如何做到这一点吧！

**怎样求字符串的长度？**

```C
#include <stdio.h>
#include <string.h>

int main() {
    // 定义一个字符串
    char str[] = "Hello, world!";
    
    // 使用strlen()函数来找到字符串的长度
    int length = strlen(str);
    
    // 输出结果
    printf("字符串的长度是：%d", length);
    return 0;
}
```

**```C预期输出：```

字符串的长度是：13

**深入探讨**

一个字符串在C语言中实际上是一个字符数组，是由一连串的ASCII码来表示的。当我们给一个字符串赋值时，C语言会自动在最后一个字符后面添加一个空字符('\0')来作为字符串的结尾。这就是为什么我们需要使用strlen()函数来找到字符串的长度。因为这个函数会从第一个字符开始，一直向后数，直到遇到空字符为止。

**查看更多**

- [C语言中的字符串](https://www.runoob.com/cprogramming/c-strings.html)
- [strlen()函数用法](https://www.programiz.com/c-programming/library-function/string.h/strlen)