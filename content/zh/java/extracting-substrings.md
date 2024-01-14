---
title:    "Java: 选取子串"
keywords: ["Java"]
---

{{< edit_this_page >}}

# 为什么要提取子串
字符串操作是Java编程中必不可少的部分。有时候，我们需要从一个字符串中抽取出一部分内容，这就是提取子串的用处。这对于处理大量文本数据或者字符串处理类的应用非常有用。

## 如何提取子串
提取子串的基本方法是使用Java中的substring方法。该方法接受两个参数，第一个参数是起始索引，第二个参数是结束索引（不包括该索引对应的字符）。

```Java
String str = "Hello World";
String subStr = str.substring(0, 5);
System.out.println(subStr);
```
输出：
```Java
Hello
```

我们也可以使用substring方法来提取字符串中的单个字符。只需将起始索引和结束索引设置为相同的值即可。例如：

```Java
String str = "Hello World";
char ch = str.substring(3, 4);
System.out.println(ch);
```
输出：
```Java
l
```

同时，我们还可以使用负数作为索引来提取子串。此时，索引的计算方式为从字符串的末尾开始，例如：

```Java
String str = "Hello World";
String subStr = str.substring(-5, -1);
System.out.println(subStr);
```
输出：
```Java
Worl
```

## 深入了解提取子串
除了基本的substring方法外，Java还提供了其他方法来提取子串。这些方法可以根据不同的需求提取子串，比如根据指定的字符或者正则表达式来提取子串。

另外，还有一些常用的字符串操作方法，比如trim去除字符串中的空格、toUpperCase和toLowerCase将字符串转换为大写或者小写、replace替换字符串中的指定字符等等。这些方法都可以在提取子串的基础上做更多的字符串处理。

# 参考链接
- [Java String类官方文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java substring方法详解](https://www.runoob.com/java/string-substring.html)
- [Java正则表达式基础教程](https://www.runoob.com/java/java-regular-expressions.html)

# 参见
- [Java字符串处理入门](https://github.com/username/java-string-processing)