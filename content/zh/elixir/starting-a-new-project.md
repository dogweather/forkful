---
title:                "开始一个新项目"
html_title:           "Elixir: 开始一个新项目"
simple_title:         "开始一个新项目"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 为什么

在开始新项目时，选择Elixir作为编程语言有很多好处。Elixir拥有一个强大的并发模型，能够轻松处理大量的数据和请求。它也具有简洁的语法和模式匹配功能，让编写代码变得更加轻松和直观。

## 如何开始

要开始一个新的Elixir项目，首先需要安装它。通过下面的命令可以在命令行上进行安装：
```Elixir
brew install elixir
```
接下来，需要创建一个新的项目目录并在其中初始化一个Elixir项目。通过以下命令可完成这一步骤：
```Elixir
mix new my_project
```
然后，进入新创建的项目目录中，并使用`mix`命令来运行项目。例如，运行下面的命令可以启动Elixir的交互式命令行：
```Elixir
mix run --no-halt
```

## 深入探讨

在开始新项目之前，建议先简单了解一下Elixir的基本语法和语言特性。例如，Elixir使用的是函数式编程范式，与其他常见的编程语言（如Java或Python）有很大的不同。这意味着Elixir中没有可变的变量，而是通过创建新的变量副本来修改它们的值。

此外，Elixir还拥有强大的模式匹配功能，可以让我们更轻松地处理数据。通过模式匹配，我们可以根据不同的条件来执行不同的代码逻辑，这在处理大量数据时非常有用。

最后，建议使用Elixir自带的文档工具`mix docs`来浏览官方文档和函数的用法示例。

## 参考资料

- [Elixir官方文档](https://elixir-lang.org/getting-started/introduction.html)
- [Elixir School中文版](https://elixirschool.com/zh-cn/)
- [Elixir入门教程](https://elixirschool.com/zh-cn/lessons/basics/basics/)
- [Elixir语言基础](https://elixir-lang.org/getting-started/introduction.html)

## 参见

- [Elixir入门指南（英文）](https://elixir-lang.org/getting-started/introduction.html)
- [Elixir官方指南（英文）](https://elixir-lang.org/docs.html)