---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

分割子串是将字符串中的一段数据取出来。它是编程中常见的需求，可以帮助我们抓取和分析文本数据。

## 怎么做：

在C语言中，我们使用`strncpy`来分割字符串。但是注意！`strncpy`并不能为新的字符串添加结束符'\0'，你需要自己添加。下面是代码片段：

```C
#include <stdio.h>
#include <string.h>

int main() {
    char input[50] = "I love Mandarin!";
    char output[10];

    strncpy(output, input + 2, 4);
    output[4] = '\0';  // 不要忘记这个！

    printf("%s\n", output);  // 打印 "love"
    
    return 0;
}
```
运行上面的代码，你可以看到输出结果是："love"，这就是我们从原始字符串中抽取的子串。

## 深入探索：

C语言的字符串处理函数起源于早期UNIX系统，而分割字符串功能一直是文本处理任务的一部分。虽然方法多样，但是用到的主要思路就是计算起始地址和长度。

此外，你还可以使用`sscanf`函数或指针操作来实现字符串的分割，只是在实际应用中应当尽可能选择最适合场景的方法。

值得注意的是，C标准库提供的函数都没有错误检查机制。所以，使用时务必确保操作是安全的，例如，复制的字符数量不能超过目标缓冲区的大小。

## 另请参阅：

这里有一些增进理解和提高技巧的资源：

1. [C语言库函数](http://www.runoob.com/cprogramming/c-standard-library.html): Runoob.com提供的C标准库函数列表和使用详解


3. [字符串的内部表示](https://en.wikipedia.org/wiki/String_interning): 认识字符串的内部表示和实现方式。