---
title:                "Java: 字符串连接"
simple_title:         "字符串连接"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/concatenating-strings.md"
---

{{< edit_this_page >}}

Java连接字符串的原因

作为Java开发人员，连接字符串（串联字符串）是一项必不可少的编程任务。它允许我们将两个或多个字符串合并为一个更长的字符串，从而实现更灵活和有效的文字处理。无论是构建用户界面还是处理文本数据，字符串连接都是非常常见的操作。

## 如何操作

在Java中，连接字符串最常用的方式是使用```+```操作符。以下是一个示例，将两个字符串连接在一起：

```
String str1 = "Hello";
String str2 = "World";
String result = str1 + str2;
System.out.println(result);
```

输出结果将是"HelloWorld"。如果需要，我们也可以在字符串中插入其他字符来分隔它们：

```
String str1 = "I";
String str2 = "love";
String str3 = "programming";
String result = str1 + " " + str2 + " " + str3;
System.out.println(result);
```

此时，输出结果将是"I love programming"。

除了```+```操作符，我们还可以使用```concat()```方法来连接字符串：

```
String str1 = "Goodbye";
String str2 = "World";
String result = str1.concat(str2);
System.out.println(result);
```

输出结果也将是"GoodbyeWorld"。然而，使用```+```操作符比使用```concat()```方法更为简洁和直观。

## 深入了解

在Java中，字符串是不可变的，这意味着一旦创建了一个字符串，它的内容将无法改变。因此，每次连接字符串时，实际上是创建了一个新的字符串对象。

由于字符串不可变，连接操作可能会导致内存浪费。每次连接都会创建一个新的字符串对象，原始字符串对象也不会被回收，最终会占用大量内存空间。为了避免这种情况，我们可以使用StringBuilder类。它允许我们在修改字符串时直接在原始对象上操作，从而避免不必要的内存消耗。

另外，当我们需要连接大量字符串时，使用StringBuilder类比使用```+```操作符或```concat()```方法更快更高效。

## 查看更多

* [Java String类文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
* [Java StringBuilder类文档](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
* [Java字符串操作教程](https://www.w3schools.com/java/java_strings.asp)