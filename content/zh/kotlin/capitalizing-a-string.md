---
title:                "将字符串大写化"
html_title:           "Kotlin: 将字符串大写化"
simple_title:         "将字符串大写化"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，我们经常需要将文本中的单词首字母大写，这样可以使文本更整洁、易于阅读。使用Kotlin的capitalize()函数可以轻松实现这一目的，让我们来看看如何使用它吧！

## 如何操作

使用capitalize()函数很简单，首先需要创建一个String类型的变量，然后调用capitalize()方法即可将其首字母大写。下面是一个示例代码和输出：

```Kotlin
val str = "kotlin is a fun and easy language"
println(str.capitalize()) //输出：Kotlin is a fun and easy language
```

同时，capitalize()函数也可以接受一个参数，用于指定首字母大写的规则，比如只将第一个单词的首字母大写，或者只将第一个单词的第一个字符大写。下面是一个示例代码和输出：

```Kotlin
val str = "kotlin is a fun and easy language"
println(str.capitalizeWords()) //输出：Kotlin Is A Fun And Easy Language
println(str.capitalizeWords(' ')) //输出：Kotlin is a fun and easy language
```

## 深入了解

了解capitalize()函数更多高级用法，可以参考官方文档，链接如下：

- [Kotlin官方文档](https://kotlinlang.org/docs/reference/)：包含capitalize()函数的具体介绍及其他String类的相关函数。
- [Kotlin Playground](https://play.kotlinlang.org/)：一个在线的Kotlin代码编辑器，可以用来练习和测试capitalize()函数的使用。
- [Kotlin中国社区](https://kotlincn.net/)：一个专注于分享Kotlin技术文章和资源的社区，可以在这里找到更多关于Kotlin的内容。

## 参考链接

- [Kotlin官方文档](https://kotlinlang.org/docs/reference/)
- [Kotlin Playground](https://play.kotlinlang.org/)
- [Kotlin中国社区](https://kotlincn.net/)