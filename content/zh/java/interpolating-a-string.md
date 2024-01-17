---
title:                "插入一个字符串"
html_title:           "Java: 插入一个字符串"
simple_title:         "插入一个字符串"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？
字符串内插是一种用来动态替换字符串中占位符的编程技巧。程序员们经常使用它来构建动态和可变的文本内容，以便根据不同的输入和条件来生成不同的输出。

## 如何：
下面是一个使用字符串内插的简单示例代码，以及预期的输出：

```Java
String name = "丽丽";
int age = 27;

// 使用字符串内插来动态构建问候语
String greeting = `您好，我的名字是${name}，我今年${age}岁。`;
System.out.println(greeting);

// 预期输出：您好，我的名字是丽丽，我今年27岁。
```

## 深入了解：
字符串内插最早是在Java 15中引入的一个新特性，以便更方便地构建动态字符串。以前，程序员们通常会使用字符串连接符“+”来拼接不同的字符串，但这种方法比较繁琐和容易出错。除了使用字符串内插，程序员们还可以使用String.format()方法来生成格式化的字符串。但它的语法比较复杂，因此在简单的情况下，字符串内插更为常用。

## 查看更多：
- [Java 15文档](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/lang/String.html#formatted-string-literals)
- [Java字符串实用程序API](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/Formatter.html#syntax)
- [Java字符串教程](https://www.w3schools.com/java/java_strings.asp)