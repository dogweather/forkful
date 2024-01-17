---
title:                "提取子字符串"
html_title:           "C: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# 前言：什么是字符串提取
字符串提取是指从一个更大的字符串中获取子串（或部分字符串）的过程。这对于程序员来说非常有用，因为它允许他们根据需要从一个大型字符串中获取特定的信息或数据，并在程序中使用。

现在，让我们深入了解字符串提取的一些技巧和技术。

## 什么 & 为什么？
字符串提取是从一个字符串中提取特定信息的过程。程序员通常在需要从输入数据中获取特定数据时使用字符串提取。例如，他们可以从用户输入的字符串中提取邮箱地址或电话号码。

为什么这么做？因为它可以帮助程序员快速准确地获取所需的信息，避免手动提取的繁琐步骤，并且可以提高代码的可读性。

## 如何：
让我们来看一个简单的例子，展示如何在C语言中提取字符串。假设我们有一个字符串“Hello World”，我们想要提取“World”作为我们的子串。

```
#include<stdio.h>

int main() {
   char str[] = "Hello World";
   char substr[6];
   // 从索引6（'W'）开始复制字符到substr数组中
   strcpy(substr, &str[6]);
   // 输出结果
   printf("提取的子串为：%s", substr);
   
   return 0;
}
```

输出结果：提取的子串为：World

## 深入了解：
字符串提取早在计算机程序设计的早期就已经出现了。在早期的编程语言中，如BASIC和FORTRAN，字符串提取通常是通过子字符串函数来实现的。随着计算机语言的发展，各种各样的字符串提取方法开始出现，如正则表达式、循环和子字符串切割。

在C语言中，我们可以使用strncpy函数来复制字符串的一部分，但是它只能复制指定长度的子串。相比之下，strcpy函数可以根据字符串的结束符来复制整个字符串。

## 查看更多：
- 子字符串函数：https://www.tutorialspoint.com/c_standard_library/c_function_strncpy.htm
- 正则表达式：https://www.tutorialspoint.com/c_standard_library/c_function_regexp.htm
- 循环：https://www.tutorialspoint.com/cprogramming/c_loops.htm
- 字符串切割：https://www.tutorialspoint.com/cprogramming/c_strings.htm