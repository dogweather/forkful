---
title:                "Java: 连接字符串"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么
拼接字符串是Java编程中常用的操作，它可以将多个字符串连接在一起，形成一个新的字符串。这对于输出和处理文本信息非常有用，让我们来看一下如何使用Java代码进行拼接字符串操作。

## 如何
在Java中，可以通过 "+" 符号来拼接字符串，也可以使用字符串的 concat() 方法。下面是一个示例代码：

```Java
String str1 = "Hello ";
String str2 = "Mandarin readers!";
String result = str1 + str2; // 使用加号拼接
String result2 = str1.concat(str2); // 使用 concat() 方法拼接
System.out.println(result); // 输出：Hello Mandarin readers!
System.out.println(result2); // 输出：Hello Mandarin readers!
```
注意，拼接的顺序取决于字符串的顺序。如果需要在字符串中插入其他内容，可以用 " + " 将它们连接起来。

```Java
String str1 = "Hello";
String name = "John";
String result = str1 + " " + name + "!"; // 使用加号和空格拼接
System.out.println(result); // 输出：Hello John!
```

## 深入探讨
在Java中，字符串是被视为不可变的，也就是说它们无法被修改。因此，每次拼接字符串都会生成一个新的字符串对象。这就意味着，每次使用加号和 concat() 方法拼接字符串，都会产生额外的内存开销。为了避免频繁地创建新的字符串对象，建议使用 StringBuilder 或 StringBuffer 类来拼接字符串。

```Java
String str1 = "Hello";
String name = "John";
StringBuilder builder = new StringBuilder(); // 创建一个可变的StringBuilder对象
builder.append(str1).append(" ").append(name); // 使用 append() 方法拼接
String result = builder.toString(); // 将StringBuilder对象转换为字符串
System.out.println(result); // 输出：Hello John!
```

另外，需要注意的是，在使用 String 类的 "+" 拼接字符串时，由于每次都会创建新的字符串对象，因此会占用较多的内存空间。而 StringBuilder 或 StringBuffer 类只会修改对象本身，不会创建新的对象，所以在处理大量字符串拼接时，建议使用可变的 StringBuilder 或 StringBuffer 类。

## 另请参阅
- [Java String - concat() Method](https://www.w3schools.com/java/ref_string_concat.asp)
- [Java String - StringBuilder](https://www.geeksforgeeks.org/java-string-builder-class/)
- [Java String concatenation (+ vs concat())](https://javarevisited.blogspot.com/2013/03/java-string-concat-example-tutorial.html)