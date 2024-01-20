---
title:                "使用正则表达式"
html_title:           "Java: 使用正则表达式"
simple_title:         "使用正则表达式"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 什么是正则表达式？为什么程序员要使用它？

正则表达式是一种在编程中常用的文本匹配工具。它可以帮助程序员快速地找到和处理文本中的特定模式，比如电话号码、电子邮箱、文档中的关键字等等。程序员使用正则表达式可以节省大量的时间和精力，同时也提高了代码的整洁性和可维护性。

## 如何使用正则表达式？

下面是一个例子，展示了如何用正则表达式在一个文本中查找所有的电子邮箱地址，并将其存储在一个字符串数组中：

```java
String text = "Please send the completed form to john@example.com or jane@example.com";

//使用正则表达式查找所有的电子邮箱地址
String regex = "[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,3}"; 
Pattern pattern = Pattern.compile(regex);
Matcher matcher = pattern.matcher(text);

//将匹配的结果存储在字符串数组中
List<String> emails = new ArrayList<>();
while (matcher.find()) {
    emails.add(matcher.group());
}

//输出结果
System.out.println("匹配到的电子邮箱地址：");
for (String email : emails) {
    System.out.println(email);
}
```

输出结果：
```
匹配到的电子邮箱地址：
john@example.com
jane@example.com
```

## 深入了解正则表达式

正则表达式最早由Unix操作系统发明，用于文本搜索和替换。它是一个强大的工具，但也有一些替代方案，比如字符串函数和搜索库。在Java中，我们使用Java自带的java.util.regex包来实现正则表达式功能。除了基本的文本匹配，正则表达式还可以实现更复杂的匹配，比如捕获组、非贪婪匹配等等。

## 更多资源

想要了解更多关于正则表达式的知识，可以参考以下资源：

- [Java正则表达式教程](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [正则表达式语法参考](https://www.rexegg.com/regex-quickstart.html)