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

<h2>为什么</h2>

提取子字符串是解决字符串相关问题的一种常见方法。它允许我们从一个字符串中提取我们需要的特定部分，使得处理字符串更加灵活和有效。

<h2>如何操作</h2>

提取子字符串可以通过Java String类提供的两种方法来实现：

1.使用substring()方法：该方法接受两个参数，用于指定要提取的子字符串的起始位置和结束位置。它将返回一个新的字符串对象，包含原始字符串中指定范围内的字符。

```Java
String str = "Hello World";
String substr = str.substring(6, 11);
System.out.println(substr); // Output: World
```

2.使用charAt()方法和for循环：这种方法需要使用charAt()方法来逐个访问字符串中的字符，并使用for循环来指定要提取的范围。在循环中，使用StringBuilder类来构建新的字符串并将每个字符添加到其中。

```Java
String str = "Hello World";
StringBuilder sb = new StringBuilder();
for(int i = 6; i < 11; i++) {
  sb.append(str.charAt(i));
}
String substr = sb.toString();
System.out.println(substr); // Output: World
```

<h2>深入了解</h2>

除了提取特定范围的子字符串外，我们还可以使用String类提供的其他方法来操作和处理字符串。一些常用的方法包括：

1.使用indexOf()方法来查找子字符串在原始字符串中的位置。它可以帮助我们快速判断一个字符串是否包含另一个字符串，并提取其位置。

2.使用trim()方法来去除字符串中的空格，非常适用于用户输入的数据处理。

3.使用split()方法根据指定的分隔符将字符串拆分成字符串数组，方便我们处理大量数据。

<h2>相关链接</h2>

- [Java String类文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java StringBuilder类文档](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)
- [更多关于字符串的常用方法](https://www.geeksforgeeks.org/java-string-class/)
- [String和StringBuilder性能比较](https://www.baeldung.com/java-string-performance)