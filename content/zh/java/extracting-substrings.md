---
title:                "提取子字符串"
html_title:           "Java: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/extracting-substrings.md"
---

{{< edit_this_page >}}

# 什么是提取子字符串？为什么程序员要这么做？

提取子字符串是指从一个较长的字符串中截取出一段短的字符串。程序员通常会这么做是为了从一个大的字符串中获取特定的信息，或者对字符串进行处理和修改。

# 如何提取子字符串：

Java中有几种方法可以提取子字符串，下面是两种常用的方法：

1. 使用 `substring()` 方法。这个方法接受两个参数，第一个参数是起始索引（从0开始计数），第二个参数是结束索引（不包括该索引的字符）。例如，假设我们有一个字符串 `"Hello World"`，想要提取出 `"Hello"` 这个子字符串，可以使用如下代码：```Java
String str = "Hello World";
String subStr = str.substring(0, 5);
System.out.println(subStr); // Output: Hello
```

2. 使用正则表达式。如果需要从字符串中提取符合某种规律的子字符串，可以使用正则表达式。例如，假设我们有一个包含不同邮件地址的字符串，想要提取出所有的用户名，可以使用如下代码：```Java
String str = "john@example.com, lisa@example.com, tom@example.com";
Pattern pattern = Pattern.compile("\\w+");
Matcher matcher = pattern.matcher(str);
while (matcher.find()) {
    System.out.println(matcher.group()); // Output: john, lisa, tom
}
```

# 深入了解：

提取子字符串在字符串处理中非常常见，它可以帮助我们简化代码，并且使得字符串的处理更加灵活。在Java中，我们可以使用更多的字符串处理方法，比如 `split()`、`replace()`、`startsWith()` 等等来达到类似的效果。另外，如果在使用 `substring()` 方法时，传入的索引超出了字符串的长度，会抛出 `IndexOutOfBoundsException` 异常。

# 参考资料：

1. [Java String class documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
2. [Java Regex tutorial](https://www.javatpoint.com/java-regex)
3. [Commonly used String methods in Java](https://www.geeksforgeeks.org/string-class-in-java/)