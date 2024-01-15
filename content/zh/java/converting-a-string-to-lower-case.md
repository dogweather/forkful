---
title:                "将字符串转换为小写"
html_title:           "Java: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么

在日常的编程中，我们经常需要将字符串转换为小写字母，这可以方便我们在匹配和比较字符串时排除大小写的差异。通过本文，你将学习如何在Java中进行字符串转换到小写的操作，让你的编程更加高效和方便。

## 如何进行字符串转换到小写

```Java
// 创建一个字符串
String s = "Hello, Mandarin Readers!";
// 使用toLowerCase()方法将字符串转换为小写
String lowercase = s.toLowerCase();
// 输出转换后的字符串
System.out.println(lowercase);
```

输出结果：

```
hello, mandarin readers!
```

如上所示，通过调用Java中的`toLowerCase()`方法，我们可以将字符串中的所有大写字母转换为小写字母。在实际的编程中，我们可以将其与`toUpperCase()`方法结合使用，来实现字符串的大小写统一。

## 深入了解字符串转换到小写的操作

在Java中，字符串的转换操作是通过Unicode码表来进行的。其中，大写字母和小写字母的Unicode码相差32，因此，通过简单地增加或减少32，就可以完成大小写转换的操作。另外，Java还提供了`toUpperCase()`和`toLowerCase()`方法以及`Character`类中的静态方法来实现字符的大小写转换。

## 参考链接

- [Java String toLowerCase() Method](https://www.w3schools.com/java/ref_string_tolowercase.asp)
- [Java Character toLowerCase() Method](https://www.w3schools.com/java/ref_character_tolowercase.asp)
- [Unicode Character Range](https://www.unicode.org/charts/)
- [Java Character Class](https://www.geeksforgeeks.org/character-class-in-java/)

## 参见

- [Java字符串操作教程](https://www.runoob.com/java/java-string.html)
- [Java字符串处理实用技巧](https://blog.csdn.net/lishuming000/article/details/51101602)
- [Unicode编码表参考](https://unicode-table.com/cn/)