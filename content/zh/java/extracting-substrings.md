---
title:    "Java: 提取子字符串"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

在编写Java程序时，有时候我们需要从一个字符串中提取出一部分内容。这可以帮助我们处理文本数据更加方便和灵活，例如搜索和替换特定的文本。因此，提取子字符串是Java编程中常用的基本操作。

## 如何做

为了提取子字符串，我们需要使用Java中的substring()方法。这个方法接受两个参数，分别是要开始提取的索引和要结束提取的索引。例如，如果我们想从一个字符串中提取出前5个字符，可以这样写：

```Java
String str = "Hello World";
String subStr = str.substring(0, 5);
System.out.println(subStr); // Output: Hello
```

需要注意的是，Java中的索引是从0开始的，所以如果想要提取第一个字符，索引应该为0。另外，如果不指定结束索引，那么substring()方法会一直提取到字符串的末尾。

我们还可以根据需要，使用substring()方法提取出多个子字符串，然后再进行处理和拼接。

## 深入了解

除了起始和结束索引，substring()方法还可以接受一个可选的第三个参数，用于指定每个提取的子字符串的长度。这样可以更灵活地提取出特定长度的子字符串，而不需要手动计算索引。

另外，需要特别注意的是，String类中的substring()方法会返回一个新的字符串对象，而不是修改原始的字符串。如果需要修改原始字符串，可以使用StringBuilder或StringBuffer类。

## 查看更多

如果想要了解更多关于提取子字符串的知识，可以参考以下链接：

- [Java String类文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Java Substring方法教程](https://www.w3schools.com/java/java_ref_string.asp)
- [Java String类视频教程](https://www.youtube.com/watch?v=V9UG5k1UjwA)

## 参考链接

- [在Java中提取子字符串的方法](https://www.codejava.net/coding/how-to-extract-a-substring-from-a-string-in-java)
- [Java String类的substring()方法详解](https://blog.csdn.net/weixin_34327244/article/details/86769831)