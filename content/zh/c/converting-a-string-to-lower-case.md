---
title:                "将字符串转换为小写"
date:                  2024-01-20T17:37:50.868135-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"

category:             "C"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
什么和为什么？
将字符串转换成小写意味着把所有英文字母从大写改为小写。程序员做这个是为了数据的一致性和比较，譬如在文本搜索中忽略大小写。

## How to:
怎么做：
```C
#include <stdio.h>
#include <ctype.h>

void convertToLowercase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char myString[] = "HeLLo, WoRLd!";
    convertToLowercase(myString);
    printf("%s\n", myString); // 输出: hello, world!
    return 0;
}
```

## Deep Dive
深入了解：
在C编程的早期阶段，就有了处理字符串的需要，而将字符串转换为小写也是基本操作之一。`tolower`函数存在于标准库`ctype.h`中，用于转换单个字符。它是ANSI C标准的一部分，几乎所有C编程环境都支持。除了利用`tolower`，也可以通过减去字符'大写A'与'小写a'之差值（通常是32）的方法来转换字符。然而，这种方式有局限性，因为它假设了编码集是连续且有序的（如ASCII），在其他编码集（如UTF-8）中不一定适用。

实现字符串的小写转换时，除了使用循环和`tolower`，也可以利用C库中的`strlwr`函数（可能不是标准C，依编译器而异），这个函数可以直接将整个字符串转为小写。虽然方便，但在不同的平台上可能会遇到兼容性问题。

## See Also
参考链接：
- C标准库函数`tolower`: http://www.cplusplus.com/reference/cctype/tolower/
- ASCII表和字符编码基础知识：https://www.ascii-code.com/
- 关于C语言字符串处理函数的更多信息：https://en.cppreference.com/w/c/string/byte
