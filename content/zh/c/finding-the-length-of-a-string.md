---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么?

查找字符串的长度就是确定字符串中字符的数量。程序员经常寻找字符串的长度，以便在排序、搜索或比较时能正确处理字符串。

## 怎么做:

以下是查找字符串长度的C语言实现，我们使用内置函数`strlen`：
```C
#include <stdio.h>
#include <string.h> 

int main() 
{ 
    char str[] = "Hello, World!"; 
    printf("The length of the string is %zu", strlen(str)); 
    return 0; 
}
```
预期输出为：
```
The length of the string is 13
```

## 深入理解:

- **历史背景**：早期编程语言（如FORTRAN、COBOL和Algol）没有提供关于字符串长度的直接方法，开发者需要自行编码找出字符串长度。C语言后来在其库函数中提供了 `strlen` 函数，用于返回字符串长度。

- **替代方案**：虽然 `strlen` 是一个很好的工具，但在某些情况下也可以用其他方法来获取字符串长度。例如，你可以创建一个循环，直到检测到空字符 ('\0')为止。

- **实现细节**： `strlen` 函数工作原理是从字符串的第一个字符开始，每次移动到下一个字符，直到找到空字符 ('\0')，返回移动的次数。

## 参见：

- C库函数: [strlen()函数](https://www.runoob.com/cprogramming/c-function-strlen.html)