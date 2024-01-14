---
title:                "Java: 将字符串转为小写"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要转换字符串为小写

在Java编程中，有时需要将字符串中的字母全部转换为小写字母。这可以帮助我们在比较字符串时忽略大小写，从而使代码更加健壮和易读。

## 如何进行转换

```Java
String str = "Hello World";
String lowerCaseStr = str.toLowerCase(); // Hello World
System.out.println(lowerCaseStr); // hello world
```

在上面的示例中，我们首先将"Hello World"字符串赋值给一个变量str，然后使用toLowerCase()方法将其转换为小写字母，并将结果赋值给lowerCaseStr变量。最后使用println()方法打印输出lowerCaseStr变量，结果为"hello world"。

## 深入了解转换字符串为小写

在Java中，toLowerCase()方法是String类的一个实例方法，用于将字符串中的所有字母转换为小写形式。它使用语言环境的默认规则来执行转换，这意味着它将根据当前系统的语言环境来转换字符串。

此外，Java还提供了toUpperCase()方法，用于将字符串中的所有字母转换为大写形式。

## 查看更多

- [Java String 文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [如何比较字符串](https://www.geeksforgeeks.org/compare-two-strings-in-java/)
- [Java 对大小写不敏感的字符串比较](https://www.baeldung.com/java-string-compare-case-insensitive)

# 查看也

- [为什么使用字符串转换为小写](https://www.programiz.com/java-programming/examples/lowercase)
- [Java 中字符串大小写转换](https://www.tutorialspoint.com/java/java_string_tolowercase.htm)
- [Java 中大小写不敏感的字串匹配](https://www.tutorialspoint.com/how-to-do-case-insensitive-string-comparison-in-java)