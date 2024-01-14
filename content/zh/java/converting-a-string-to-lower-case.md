---
title:    "Java: 将字符串转换为小写"
keywords: ["Java"]
---

{{< edit_this_page >}}

为什么要将字符串转换为小写

在编程中，经常会遇到需要将字符串转换为小写的情况。这有助于统一字符串的格式，方便后续的比较和处理。下面将介绍如何在Java中将字符串转换为小写，以及一些更深入的知识。

如何操作

首先，我们需要使用Java内置的toLowerCase()方法来实现字符串小写的转换。具体的语法为：

```Java
String str = "Hello World";
String result = str.toLowerCase();
System.out.println(result);
```

运行以上代码，将会得到输出结果为："hello world"。

除了使用内置方法，我们也可以使用循环结构来实现字符串中每个字符的小写转换。具体的代码如下：

```Java
String str = "Hello World";
String result = "";
for (int i = 0; i < str.length(); i++) {
    char c = str.charAt(i);
    if (Character.isUpperCase(c)) {
        result += Character.toLowerCase(c);
    } else {
        result += c;
    }
}
System.out.println(result);
```

运行以上代码，同样会得到输出结果为："hello world"。

深入了解

在Java中，字符串是不可变的对象，这意味着原始的字符串不会被改变，而是通过创建一个新的字符串来实现小写转换。另外，Java中的字符串使用的是Unicode编码，因此无论输入的是中文、英文还是其他语言，都可以正确地转换为小写。

此外，对于某些特殊情况下，可以使用Locale类来实现更精确的小写转换。例如，针对土耳其语言，小写转换会有特殊的规则，此时可以使用Turkey Locale来处理。具体的代码如下：

```Java
String str = "Istanbul";
String result = str.toLowerCase(new Locale("tr", "TR"));
System.out.println(result);
```

输出结果为："ıstanbul"，符合土耳其语言的小写转换规则。

另外，注意在比较字符串时，通常需要将两个字符串都转换为小写，以避免大小写的差异导致比较结果不准确。

总结

如此简单的方法就可以将字符串转换为小写，无论是使用内置方法，还是自己写循环，都能轻松实现。另外，如果遇到特殊情况，也可以使用Locale类来处理更精准的转换。

请参考

- [Java官方文档 - String类](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Oracle - Locale类](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html)
- [Stack Overflow - Converting a String to Lowercase in Java](https://stackoverflow.com/questions/9399001/converting-a-string-to-lower-case-in-java)