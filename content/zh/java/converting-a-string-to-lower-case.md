---
title:                "将字符串转换为小写"
html_title:           "Arduino: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 什么和为什么?

字符串转换为小写就是把字符串中的所有大写字母改成小写字母。转换的主要原因是为了进行文本比较和搜索，因为这些操作在处理大小写时会发生误差。

## 如何实现:

以下是将字符串转换为小写的一种常见方法：

```Java
String string = "HELLO WORLD!";
String lowerString = string.toLowerCase();
System.out.println(lowerString);
```

输出结果会是:

```
hello world!
```

你只需调用`toLowerCase()`方法，就可以把任何字符串转换为全部小写。

## 深度解读:

- 历史背景：早期的计算机系统对大小写敏感，为了减少误差，程序员开始将文本转换为小写。Java在1.0版就引入了`toLowerCase()`方法。
- 替代方案：在特定情况下，你也可以使用`String.toLowerCase(Locale.ROOT)`来避免因某些特殊语系引发的问题。
- 实现细节：`toLowerCase()`方法会查找字符串中每个字符的小写等价项。如果没有找到，字符本身就会被返回。

## 参考链接:

更多关于字符串如何转换为小写的信息，你可以查阅以下链接:

1. Oracle Java 文档: [String.toLowerCase()](https://docs.oracle.com/en/java/javase/13/docs/api/java.base/java/lang/String.html#toLowerCase())
2. W3Schools 教程: [Java String toLowerCase() Method](https://www.w3schools.com/java/ref_string_tolowercase.asp)