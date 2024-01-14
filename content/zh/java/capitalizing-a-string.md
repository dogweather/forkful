---
title:                "Java: 将字符串大写化"
simple_title:         "将字符串大写化"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/capitalizing-a-string.md"
---

{{< edit_this_page >}}

为什么要使用字符串大写

通常，人们需要将字符串大写是为了统一格式或者符合特定的规范。在编程中，字符串大写也可以方便不同的编程任务，如用户输入的验证或者比较字符串。

## 如何实现字符串大写

在Java中，可以使用String类的toUpperCase()方法来将字符串转换为大写。下面是一个简单的示例代码：

```Java
// 创建一个字符串
String message = "hello world";

// 转换为大写
String upperCaseMessage = message.toUpperCase();

// 输出结果
System.out.println(upperCaseMessage); // HELLO WORLD
```

## 深入了解字符串大写

在Java中，字符串是不可变的，也就是说无法直接修改原始字符串。当调用toUpperCase()方法后，会返回一个新的字符串对象，该对象包含原始字符串的大写版本。这是由于Java使用Unicode字符集，它包含大写和小写版本的每个字符。

另外，当处理非英语字符时，需要注意使用Locale参数来指定正确的语言环境。例如：

```Java
// 指定德语语言环境
String germanMessage = "Guten Tag";
String germanUpperCase = germanMessage.toUpperCase(Locale.GERMAN);

// 输出结果
System.out.println(germanUpperCase); // GUTEN TAG
```

## 参考链接

- [Java String类文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [String toUpperCase()方法文档](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toUpperCase-java.util.Locale-)