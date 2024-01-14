---
title:                "Java: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# 为什么要把字符串转换为小写

当处理字符串时，有时候我们需要把所有的字母转换成小写。这可能是因为我们需要比较字符串而不区分大小写，或者是为了美观的输出。无论是什么原因，学会如何把字符串转换为小写都是非常有用的技巧。

## 如何实现

要把一个字符串转换为小写，我们可以使用Java内置的toLowerCase()方法。该方法会返回一个新的字符串，其中所有的大写字母都被转换为小写。下面是一个例子：

```Java
String str = "Hello, World!";
String result = str.toLowerCase();
System.out.println(result);
```

运行以上代码会输出 `hello, world!`。由于字符串是不可变的，所以转换后的结果需要保存在一个新的变量中。

除了使用内置的toLowerCase()方法，我们也可以使用ASCII码来实现字符串转换为小写。ASCII码是一个美国标准信息交换码，它使用数字来表示字母、数字、标点符号等字符。在ASCII码表中，小写字母和大写字母之间相差32。因此，我们可以通过将字符串中的每个字符的ASCII码加32来实现转换。下面是使用ASCII码转换字符串的一个例子：

```Java
String str = "Hello, World!";
String result = "";
for (int i = 0; i < str.length(); i++) {
    char c = str.charAt(i);
    if (c >= 'A' && c <= 'Z') {
        result += (char) (c + 32); // 将字符的ASCII码加32并转换为字符
    } else {
        result += c;
    }
}
System.out.println(result);
```

运行以上代码会输出 `hello, world!`。这种方法的好处是可以更好地理解字符串的结构和ASCII码的含义。

## 深入了解

在了解如何把字符串转换为小写后，我们还可以探索一些额外的知识。在Java中，字符串是通过String类来表示的，它是一个不可变的对象。这意味着当我们对字符串做出修改时，实际上是创建了一个新的字符串。因此，在进行大量的字符串操作时，考虑到内存的使用是非常重要的。

此外，String类还提供了一些其他有用的方法来操作字符串，如比较字符串、拆分字符串、截取子串等。如果想要更加深入地学习字符串的操作，可以查阅Java官方文档或参考其他相关资料。

# 参考链接

- [Java官方文档 - String](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/lang/String.html)
- [百度百科 - ASCII码](https://baike.baidu.com/item/ASCII%E7%A0%81/309296?fr=aladdin)

# 同时查看

- [Java教程 - 字符串操作方法](https://www.runoob.com/java/java-string.html)
- [Java教程 - 字符串](https://www.w3cschool.cn/java/java-string.html)