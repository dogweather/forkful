---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么 ? (What & Why?)

字符串插值使程序员可以在字符串中直接嵌入变量。这样做可以让读代码、编写代码的人更容易理解其含义。

```java
int apples = 5;
String msg = "我有 " + apples + " 个苹果.";
```

## 如何: (How to:)

在Java的`printf`或者`String.format`函数中，可以使用`%s`为任何变量准备占位符。

```java
String name = "Alice";
System.out.printf("Hello, %s!\n", name);  // "Hello, Alice!"
```

或者使用`%d`为数字预备占位符。

```java
int oranges = 7;
System.out.printf("你有 %d 个橙子.\n", oranges);  // "你有 7 个橙子."
```

## 深度探索 (Deep Dive)

字符串插值很早就在其他语言如Perl和Ruby中出现了，但在Java中，直到Java 1.5的`String.format`和`printf`才出现。

数组和列表等集合类的字符串插值比较复杂。一般需要使用 `String.join` 或者流 (stream) 。 

```java
List<String> words = Arrays.asList("I", "love", "Java");
String sentence = String.join(" ", words);
System.out.println(sentence);  // "I love Java"
```

可能你会看见有用 `+` 连接字符串的例子，但这种方式经常在复杂情况下产生效率问题。

## 延伸阅读 (See Also)

- Oracle 的 [printf 方法文档](https://docs.oracle.com/javase/8/docs/api/java/util/Formatter.html#syntax)
- Oracle 的 [String.format 方法文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#format-java.lang.String-java.lang.Object...-)