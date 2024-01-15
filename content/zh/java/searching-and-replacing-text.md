---
title:                "搜索和替换文字"
html_title:           "Java: 搜索和替换文字"
simple_title:         "搜索和替换文字"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 为什么

文本搜索和替换是编程中常见的任务，可以帮助提高代码的可读性和效率。通过搜索和替换文本，可以快速更改代码中的指定内容，节省时间和精力。

## 如何使用

文本搜索和替换是Java中一个基本功能，可以通过使用String类的replace()方法来实现。

```
String str = "Hello World!";
String newStr = str.replace("Hello", "你好");
System.out.println(newStr);
```

运行后的输出结果为：你好 World!

我们也可以使用正则表达式来进行更复杂的文本搜索和替换。例如，想要将字符串中的所有数字替换为"#"，可以使用以下代码：

```
String str = "Hello 123 World!";
String newStr = str.replaceAll("\\d", "#");
System.out.println(newStr);
```

运行后的输出结果为：Hello ### World!

## 深入了解

在Java中，字符串是一个不可变的对象，意味着每次执行替换操作时，都会返回一个新的字符串。因此，如果在循环中频繁使用replace()方法，会带来性能上的损耗。为了避免这种情况，可以使用StringBuilder类来实现替换操作。

```
StringBuilder sb = new StringBuilder("Hello World!");
sb.replace(0, 5, "你好");
System.out.println(sb.toString());
```

运行后的输出结果为：你好 World!

## 参考链接

- [Java String replace()方法文档](https://docs.oracle.com/javase/10/docs/api/java/lang/String.html#replace(java.lang.CharSequence,%20java.lang.CharSequence))
- [Java Pattern类文档](https://docs.oracle.com/javase/10/docs/api/java/util/regex/Pattern.html)