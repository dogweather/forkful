---
title:                "Java: 从计算机编程“提取子字符串”提取子字符串"
simple_title:         "从计算机编程“提取子字符串”提取子字符串"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/extracting-substrings.md"
---

{{< edit_this_page >}}

## 为什么

Substring，也叫子串，是指从一个字符串中截取的一段连续字符序列。通过提取子串，可以方便地获取字符串中的特定信息，比如从一个网址中提取域名或者从一个电话号码中提取区号。在日常的编程中，提取子串是一项非常常见的操作，学习这项技能可以让你更加高效地处理字符串。

## 如何提取子串

提取子串的核心方法是使用Java提供的substring()方法。该方法接受两个参数，第一个参数是起始索引，第二个参数是结束索引（不包含在提取的子串中）。下面是一个简单的例子，演示如何使用substring()方法截取字符串中的一部分内容：

```Java
String str = "Hello World";
String subStr = str.substring(0, 5); // 截取从索引0到5的字符（不包含索引5）
System.out.println(subStr); // 输出 "Hello"
```
输出结果为：Hello

您也可以使用substring()方法来提取字符串中的最后几个字符。只需要将第一个参数设置为总长度减去想要提取的字符数：

```Java
String str = "Hello World";
String subStr = str.substring(str.length() - 5); // 提取最后5个字符
System.out.println(subStr); // 输出 "World"
```
输出结果为：World

## 深入了解

除了直接提取固定位置的子串外，还可以通过一些技巧来提取特定的信息。比如，提取一个电话号码中的区号可以通过以下方法：

```Java
String phoneNumber = "(555) 123-4567";
String areaCode = phoneNumber.substring(1, 4); // 提取区号
System.out.println(areaCode); // 输出 "555"
```
输出结果为：555

值得注意的是，substring()方法也可以用来提取单个字符，只需将起始和结束索引设置为相同的值即可。

## 参考链接

- [Java String class documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [W3Schools - Java Substrings](https://www.w3schools.com/java/java_strings_substrings.asp)
- [TutorialsPoint - Java substring() method](https://www.tutorialspoint.com/java/java_string_substring.htm)

## 请参阅

- [Java字符串 - 长度，比较和连接](https://example.com)
- [Java正则表达式入门指南](https://example.com)