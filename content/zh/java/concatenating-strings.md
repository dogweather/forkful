---
title:                "串联字符串"
html_title:           "Java: 串联字符串"
simple_title:         "串联字符串"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## 为什么？

在编程中，有时需要将几个字符串连接在一起来形成一个更复杂的字符串。这可能是为了显示用户友好的信息，拼接文件路径或构建数据库查询等。通过连接字符串，程序可以更好地处理和管理数据。

## 如何做？

```Java
String text1 = "Hello";
String text2 = "world";
String result = text1 + " " + text2;
System.out.println(result);
```

输出是：Hello world

字符串拼接在Java中非常简单，只需要使用加号运算符（+）将两个字符串连接在一起即可。在上面的例子中，我们首先创建了两个字符串“Hello”和“world”，然后使用加号运算符将它们连接在一起，最后将结果打印出来。

如果想要在字符串中插入变量的值，可以使用占位符“%s”来代替变量，并在字符串结尾加上“format”方法来指定变量的值。

```Java
String name = "John";
int age = 25;
String result = String.format("My name is %s and I am %d years old.", name, age);
System.out.println(result);
```

输出是：My name is John and I am 25 years old.

## 深入了解

在Java中，字符串是不可变的，这意味着一旦创建就不能修改它们。因此，每次拼接字符串时，都会创建一个新的字符串对象，这可能会降低程序的性能。为了避免这种情况，可以使用“StringBuilder”类来拼接字符串，它允许在原始字符串上进行修改而不需要创建新的字符串对象。

```Java
StringBuilder stringBuilder = new StringBuilder();
stringBuilder.append("Hello");
stringBuilder.append(" ");
stringBuilder.append("world");
System.out.println(stringBuilder.toString());
```

输出是：Hello world

## 参考链接

- [Oracle官方文档 - 字符串拼接](https://docs.oracle.com/javase/tutorial/java/data/strings.html)
- [W3School - Java字符串拼接](https://www.w3schools.com/java/java_strings_concatenation.asp)
- [Java知识库 - 使用StringBuilder拼接字符串](https://javadoop.com/post/stringbuffer-and-stringbuilder)