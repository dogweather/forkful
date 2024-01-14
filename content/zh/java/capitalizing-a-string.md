---
title:                "Java: 将字符串转换为大写"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

为什么：有两三句话解释为什么会想要将字符串大写化。

为什么：当我们处理文本信息时，有时候会需要将字符串中的所有字符变成大写字母。这可以帮助我们更容易地比较和搜索文本内容。

如何做：首先，我们需要使用Java内置的String.toUpperCase()方法来将字符串变为全部大写。例如，我们有一个字符串变量str，我们可以使用下面的代码将其大写化：

```Java
String str = "hello world";
str = str.toUpperCase();
```

这样，str变量的值将变为"HELLO WORLD"。如果我们想要将字符串中的某些字符变为大写，而保留其他字符不变，可以使用String.substring()方法和String.toUpperCase()方法结合。例如，我们想要将字符串"hello world"中的第一个字母变为大写，可以使用下面的代码：

```Java
String str = "hello world";
str = str.substring(0, 1).toUpperCase() + str.substring(1);
```

这样，输出的str将会变为"Hello world"。

深入探讨：字符串大写化涉及到了Java中字符串处理的一些基础知识，如String类和其内置的方法。在Java中，字符串是不可变的，因此当我们对字符串进行任何修改时，实际上都是创建了一个新的字符串对象。因此在将字符串大写化时，我们实际上是创建了一个新的大写化的字符串对象，并将其赋值给原来的字符串变量。这样做的好处是保证了字符串的不可变性，从而避免在程序运行中造成意想不到的错误。

参考资料：https://www.geeksforgeeks.org/java-string-touppercase-method-example/ https://www.programiz.com/java-programming/library/string/substring https://www.javatpoint.com/java-string-toUpperCase

参见：查看更多关于Java中字符串处理的方法：https://www.w3schools.com/java/java_strings.asp