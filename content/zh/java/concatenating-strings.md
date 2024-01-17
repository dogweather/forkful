---
title:                "连接字符串"
html_title:           "Java: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/concatenating-strings.md"
---

{{< edit_this_page >}}

# 什么是字符串连接，为什么程序员要这样做？

字符串连接是将多个字符串合并为一个更长的字符串的过程。程序员经常这样做是因为在程序中需要使用大量的文本信息，将它们连接起来可以使代码更简洁和可读。

## 如何实现：

```
Java String firstName = "John";
String lastName = "Doe";
String fullName = firstName + " " + lastName;

System.out.println(fullName);
```

输出结果：John Doe

## 深入了解：

字符串连接是在早期的编程语言中出现的一项重要技术，比如C语言。它可以通过使用strcpy()函数来实现。除了"+"操作符外，Java中还有其他几种方式来实现字符串连接，比如使用StringBuffer和StringBuilder类。

## 参考资料：

了解更多关于Java中的字符串处理，可以参考以下链接：

- [Java String 类](https://www.runoob.com/java/java-string.html)
- [Java StringBuffer 类](https://www.runoob.com/java/java-stringbuffer.html)
- [Java StringBuilder 类](https://www.runoob.com/java/java-stringbuilder.html)