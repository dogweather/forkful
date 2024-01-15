---
title:                "寻找字符串的长度。"
html_title:           "Java: 寻找字符串的长度。"
simple_title:         "寻找字符串的长度。"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么
 
 字符串的长度一直是程序员们经常需要知道的信息。无论是用来验证用户输入的有效性，还是用来处理文本数据，找到字符串的长度都是非常有用的。通过这篇文章，你将学会如何使用Java来找到字符串的长度，并且你也会了解到一些更深层次的知识。

## 如何

要找到一个字符串的长度，我们可以使用String类中的length()方法。下面是一个简单的示例代码，展示了如何使用这个方法：
```Java
String str = "Hello World";
int length = str.length();
System.out.println(length); // 输出：11
```

在这段代码中，我们首先创建一个字符串变量str，它的值为"Hello World"。然后，我们使用length()方法来获取这个字符串的长度，并将其赋值给一个整数变量length。最后，我们通过打印语句来输出这个字符串的长度。

除了length()方法之外，我们也可以使用String的length属性来获取字符串的长度，效果是一样的：
```Java
String str = "Hello World";
int length = str.length;
System.out.println(length); // 输出：11
```

需要注意的是，字符串的长度不包括其中的空格。

## 深入探讨

字符串的长度实际上是它内部字符的数量。在Java中，字符串是由一个字符数组来存储的，而length()方法就是通过访问这个字符数组的长度来计算字符串的长度。

在计算字符串长度的过程中，有一些特殊情况需要注意。首先，Unicode字符会占用两个字符的位置，因此它们会使字符串的长度变长。另外，特殊字符比如回车符("\n")和制表符("\t")只占用一个字符的位置，但是它们在输出时会被展示成多个字符的样式，这也会影响字符串的长度计算。

另外，如果我们想要获取的字符串的真正长度，我们可以使用codePointCount()方法。这个方法会忽略掉Unicode字符的长度，因此可以得到正确的长度值。下面是一个例子：
```Java
String str = "Hello, 你好";
int length = str.length(); // 输出：8
int realLength = str.codePointCount(0, str.length()); // 输出：6
```

## 另请参阅

- [String类文档](http://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java编程入门教程](https://www.w3schools.com/java/)