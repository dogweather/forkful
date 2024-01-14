---
title:                "Java: 发现字符串的长度"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么：
为什么会想要获取字符串的长度？在Java编程中，字符串是非常常见的数据类型，而且在很多情况下需要对字符串的长度进行操作。比如，在验证用户输入时，往往需要检查输入的字符串是否符合指定的长度。因此，在掌握如何获取字符串长度的方法之后，能够更加灵活地处理字符串数据，在Java编程中会更加便捷。

## 如何进行：
Java提供了一个简单的方法来获取字符串的长度。我们可以使用`length()`方法来获取指定字符串的长度，并将其赋值给一个整型变量。下面是一个简单的示例代码，展示了如何使用`length()`方法来获取字符串的长度：

```Java
String str = "Hello World";
int length = str.length();
System.out.println("字符串的长度为：" + length);
```

运行以上代码，会得到以下输出结果：

```
字符串的长度为：11
```

除了使用`length()`方法，我们还可以使用字符串的属性`length`来获取其长度。如下所示：

```Java
String str = "Hello World";
int length = str.length;
System.out.println("字符串的长度为：" + length);
```

同样地，以上代码也会输出字符串的长度为11。

## 深入探讨：
在Java中，字符串是一个类（String class）。 `length()`方法实际上是该类的一个成员方法，它可以帮助我们获取字符串的长度。该方法返回一个整型值（int），代表字符串中字符的数量。需要注意的是，由于Java中的字符串是不可变的（immutable），因此一旦创建，其长度就无法改变。

另外，我们还可以使用`isEmpty()`方法来判断一个字符串是否为空。当字符串的长度为0时，即为空。下面是一个示例：

```Java
String str = "";
boolean isEmpty = str.isEmpty();
System.out.println("该字符串是否为空：" + isEmpty);
```

运行以上代码，会得到以下输出结果：

```
该字符串是否为空：true
```

## 参考链接：
- Java String类文档：https://docs.oracle.com/javase/10/docs/api/java/lang/String.html
- Java String类教程：https://www.runoob.com/java/java-string.html
- Java字符串相关操作方法：https://www.cnblogs.com/lvchaoshun/p/9870537.html

## 参见：
- Java数据类型文档：https://docs.oracle.com/javase/tutorial/java/nutsandbolts/datatypes.html
- Java字符串常用操作方法：https://www.geeksforgeeks.org/string-class-in-java/
- Java字符串处理工具包：https://commons.apache.org/proper/commons-lang/apidocs/org/apache/commons/lang3/StringUtils.html