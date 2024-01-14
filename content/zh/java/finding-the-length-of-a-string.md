---
title:                "Java: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

找出一个字符串的长度对于Java编程师来说是一个非常基础的任务。它可以帮助我们在处理字符串时更加高效和准确，比如判断用户输入是否符合要求，以及截取和拼接字符串等。

## 如何

在Java中，我们可以使用`length()`方法来找到字符串的长度。该方法是String类的一个内置方法，所以我们不需要再去导入其他的库。

```Java
String str = "这是一个测试字符串";
int length = str.length(); // 将字符串长度赋值给一个int类型的变量
System.out.println(length); // 输出：9 (中文字符占两个字节，英文字符占一个字节)
```

我们也可以通过遍历字符串的每个字符，来计算字符串的长度。这种方法比较繁琐，但也可以实现同样的功能。

```Java
String str = "这是一个测试字符串";
int count = 0;
for (int i = 0; i < str.length(); i++) {
    count++; // 每遍历一个字符，计数加一
}
System.out.println(count); // 输出：9
```

如果我们想要去除字符串中的空格，再计算长度，可以使用`trim()`方法。这样可以避免空格对计算结果的影响。

```Java
String str = " 这是一个测试字符串 ";
int length = str.trim().length(); // 使用trim()方法去除空格，再计算长度
System.out.println(length); // 输出：9
```

## 深入探究

在Java中，字符串是一个对象，因此它具有很多内置方法和属性。通过调用`length()`方法，我们可以获取字符串对象的长度属性。在计算字符串长度时，需要注意不同语言字符所占的字节数不同，因此会影响结果。

## 参考资料

- [官方Java文档 - String类](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html)
- [《Java核心技术》 第八版 - 第6章 字符串](https://book.douban.com/subject/25762168/)