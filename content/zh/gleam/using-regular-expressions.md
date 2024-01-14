---
title:    "Gleam: 使用正则表达式"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## 为什么要使用正则表达式？

正则表达式是一种强大的文本处理工具，它可以帮助程序员快速且精确地从文本中提取信息。它被广泛应用于数据清洗、字符串匹配和文本分析等领域。使用正则表达式可以大大提高编程效率，节省时间。

## 怎么做？

下面是一个简单的例子，展示如何在Gleam中使用正则表达式来匹配一个邮箱地址：

```Gleam
let regex = Regex.new("^\\w+@[a-z]+\\.[a-z]+")
let email = "example123@gmail.com"
let match = Regex.match(email, regex)
match == Ok("example123@gmail.com") // true
```

使用`Regex.new()`函数创建一个正则表达式对象，并使用`^`和`+`来定义邮箱地址的格式。然后使用`Regex.match()`函数来匹配`email`变量中的文本，返回一个包含匹配信息的`Result`类型对象。

## 深入了解

正则表达式中有许多特殊字符和符号，可以通过组合它们来创建各种不同的匹配规则。比如，使用`[]`来匹配一组字母或数字，使用`()`来创建匹配组。正则表达式还可以使用修饰符来限定匹配的范围，例如`g`表示全局匹配，`i`表示忽略大小写。

## 参考链接

- [Gleam官方文档](https://gleam.run/)
- [正则表达式语法教程](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)
- [正则表达式在线测试工具](https://regexr.com/)