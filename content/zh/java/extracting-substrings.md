---
title:                "Java: 提取子串"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/extracting-substrings.md"
---

{{< edit_this_page >}}

##为什么

提取子字符串是一项在Java编程中非常有用的技术。它可以让您从一个大字符串中获取特定部分的信息，使您的代码更加灵活和可读。让我们来看看如何在Java中提取子字符串。

##如何进行提取

为了提取子字符串，我们使用Java中内置的字符串方法 `substring()`。它需要两个参数：起始索引和结束索引。起始索引是您想要提取子字符串的起始位置，而结束索引是您想要提取子字符串的结束位置。让我们来看一个例子:

```java
String str = "Hello World!";
String subStr = str.substring(6,11);
System.out.println(subStr);
```
输出:
```
World
```

在上面的例子中，`substring()`方法从索引6到索引11提取了子字符串，并将其存储在`subStr`变量中。我们可以使用这个方法来提取任意长度的子字符串，只需调整起始和结束索引即可。

##深入了解

除了指定起始和结束索引，我们也可以使用`substring()`方法来提取从指定索引开始到字符串末尾的子字符串，如下所示:

```java
String str = "Java is an amazing programming language!";
String subStr = str.substring(11);
System.out.println(subStr);
```

输出:
```
amazing programming language!
```

我们也可以将`substring()`方法的第二个参数省略不写，这样它会自动将字符串的长度作为结束索引。这对于我们想要提取从指定位置到字符串末尾的子字符串很有用。

此外，我们还可以使用`substring()`方法来提取包含特定字符串的子字符串，如下所示:

```java
String str = "I love to code";
String subStr = str.substring(7,12);
System.out.println(subStr);
```

输出:
```
code
```

这里我们使用`substring()`方法提取了包含特定单词“code”的子字符串，它可以帮助我们从一个字符串中提取出我们需要的内容。

##参考资料

- [Java String docs](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html)
- [Java substring method](https://www.geeksforgeeks.org/java-string-substring-function-with-examples/)
- [Java substring online](https://www.javatpoint.com/java-string-substring)
- [Substring tutorial](https://www.baeldung.com/java-string-substring)
- [Substring vs Subsequence](https://stackoverflow.com/questions/19758987/difference-between-substring-subsequence-and-sublist)

##另请参阅

- [Markdown cheat sheet](https://www.markdownguide.org/cheat-sheet/)
- [Java学习资源](https://www.cs.rochester.edu/u/nelson/courses/csc_162/resources.html)
- [Java编程入门指南](https://www.ntu.edu.sg/home/ehchua/programming/index.html)