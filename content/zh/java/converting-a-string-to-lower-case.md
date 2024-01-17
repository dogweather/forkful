---
title:                "将字符串转换为小写"
html_title:           "Java: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么&为什么?

字符串是由一个字符序列组成的数据类型，在编程中经常会遇到需要将字符串中的字符转换为小写的情况。这可以通过使用Java内置的toLowerCase()方法来实现。程序员们经常进行字符串转换为小写的操作，这样可以使字符串的比较和处理更加方便和准确。

## 怎样做:

```
String str = "HELLO WORLD"; //定义一个字符串
System.out.println(str.toLowerCase()); //使用toLowerCase()方法将字符串转换为小写
```
输出结果为："hello world"

## 深入探讨:

1. 历史背景：随着计算机的普及和发展，字符串的使用变得越来越广泛。早期的计算机系统中，字符串并没有区分大小写的概念，直到后来发展出了大小写敏感的系统，字符串转换为小写的需求才变得更加重要。

2. 其他方法：除了使用Java内置的toLowerCase()方法，程序员也可以通过使用循环和if语句来实现字符串大小写的转换。但是相比之下，使用内置方法更加简洁和高效。

3. 实现细节：toLowerCase()方法是通过将字符串中的每个字符转换为其对应的小写字符来实现的。它可以同时处理英文字母和其他语言的字符，比如中文和日文。

## 参考资料:

- Java官方文档：https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--
- CSDN博客：https://blog.csdn.net/xiaoping0915/article/details/5588559