---
title:    "Java: 字符串转换为小写"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

当我们编写Java程序时，有时候我们需要将字符串转换为小写。这可能是因为我们需要与其他字符串进行比较，或者我们想要保证用户输入的数据格式统一性。转换字符串到小写也有助于简化我们的代码逻辑。

## 如何做

要将字符串转换为小写，我们可以使用Java中内置的`toLowerCase()`方法。该方法接受一个字符串作为参数，并返回一个新的字符串，其中所有的英文字母都被转换为小写。

```Java
String str = "Hello World!";
String lowerCaseStr = str.toLowerCase();
System.out.println(lowerCaseStr);
```

输出：
```
hello world!
```

## 深入探讨

将字符串转换为小写并不是一个复杂的操作，但是了解它的一些背后的原理可以帮助我们更有效地使用它。

首先，我们需要明确小写和大写字母之间的对应关系。在ASCII字符集中，大写字母的值比小写字母大32，所以当我们想要将大写字母转换为小写时，只需要将其值加上32即可。

其次，我们需要注意的是Java中的字符串是不可变的，也就是说，当我们调用`toLowerCase()`方法时，它实际上是返回一个新的字符串，而不是修改原始的字符串。这是因为Java中的字符串是被当作对象来处理的，所以当我们修改一个字符串时，实际上是创建了一个新的字符串对象。

最后，我们可以使用`toUpperCase()`方法将字符串转换为大写，它的原理与`toLowerCase()`方法相同，只是将大写字母的值减去32即可。

## 参考链接

- [Java String Conversions: toUpperCase() and toLowerCase()](https://www.geeksforgeeks.org/java-string-conversions-touppercase-and-tolowercase/)
- [Java String toLowerCase() method](https://www.javatpoint.com/java-string-tolowercase)
- [Understanding Strings and their Immutability in Java](https://www.baeldung.com/java-string-immutable)

## 参见

- [Java字符串基础知识](https://www.runoob.com/java/java-string.html)
- [Java字符串API文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)