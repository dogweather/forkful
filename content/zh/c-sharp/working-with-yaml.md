---
title:                "使用yaml进行编程"
html_title:           "C#: 使用yaml进行编程"
simple_title:         "使用yaml进行编程"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/working-with-yaml.md"
---

{{< edit_this_page >}}

## 什么是 YAML，为什么要用它？
YAML 是一种文本格式，用于保存和传输数据。它可以轻松地读写和理解，并且被广泛用于软件开发中。程序员使用 YAML 是为了方便地定义和传递数据，同时保持文本文件的易读性。

## 如何使用 YAML:
让我们来看一个简单的例子，假设我们想要保存一些学生的信息，包括他们的姓名和年龄。使用 YAML，我们可以这样定义数据：

```C#
students:
- name: John
  age: 18
- name: Emily
  age: 20
```

这样，我们就可以轻松地以结构化的方式读取学生的信息。假设我们想要输出 Emily 的年龄，我们只需要用一行代码就可以实现：

```C#
Console.WriteLine(students[1].age);
```

输出结果将会是 `20`。

## 深入了解：
历史上，程序员使用 XML 格式来存储和传输数据。然而，XML 格式的语法繁琐，使得它不易于阅读和编写。YAML 的出现解决了这个问题，它是一种更加简洁、易于理解的替代方案。在 .NET Framework 中，我们可以通过安装 `YamlDotNet.Core` 包来使用 YAML。

## 查看更多：
- [YAML 官方文档](https://yaml.org/spec/1.2/spec.html)