---
title:                "连接字符串"
html_title:           "C: 连接字符串"
simple_title:         "连接字符串"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/concatenating-strings.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
串联字符串就是将两个或多个字符串拼接到一起。程序员进行拼接主要是为了整合和处理文字信息。

## 如何进行：
以下是使用Java串联字符串的示例及其输出：

```java
public class Test {
    public static void main(String[] args) {
        String str1 = "Hello, ";
        String str2 = "World!";
        String str3 = str1.concat(str2);
        System.out.println(str3);
    }
}
```
以上代码的输出为：

```java
Hello, World!
```

## 深度剖析
从历史上看,字符串串联一直以来都是程序设计中常见的操作，它可以使代码清晰简洁，提高可读性。

然而,Java中的语句`String str3 = str1 + str2;`虽然看起来简单，但实际上在背后进行了很多操作。因为String是不可变的，每次你用"+"操作符连接字符串，Java实际上会创建一个新的字符串对象，这会显著地影响性能。因此，在处理大量字符串连接操作时，通常建议使用StringBuilder或StringBuffer。

看看使用StringBuilder的例子：

```Java
public class Test {
    public static void main(String[] args) {
        StringBuilder str = new StringBuilder("Hello, ");
        str.append("World!");
        System.out.println(str);
    }
}
```
你会看到相同的输出：“Hello, World!”

## 另请查阅
1. Java字符串参考: https://docs.oracle.com/javase/tutorial/java/data/strings.html
2. Java StringBuilder类: https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html
3. Java StringBuffer class: https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuffer.html