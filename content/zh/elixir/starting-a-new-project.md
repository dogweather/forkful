---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?
启动新项目是指创建一个新的编程工程，开始一个全新的创作过程。程序员之所以这样做，是为了解决新问题，提供新的产品或服务。

## 如何:
使用Elixir中的mix工具，可以轻松创建新的项目。如下是创建新项目的代码示例。

```Elixir
# 启动新的Elixir项目
mix new my_new_project
cd my_new_project
```

执行这个命令后，你的新项目文件夹中包含以下内容:

```Elixir
#_ls
lib  mix.exs  README.md  test
```

其中“lib”目录用于存放源代码，“test”目录用于存放测试代码，“mix.exs”则是项目配置文件。

## 深度挖掘:
Elixir自从其创立之初，就以并发或分布式编程和容错性为其主要目标，因此使得Elixir极度适合云计算和web开发。
还有其他一些工具可以创建新项目，例如Phoenix是构建在Elixir基础上的一个Web框架，你可以使用Phoenix创建一个新的web应用程序。
至于实现细节，Elixir的原则是让每个项目都被组织成小的，独立的应用，从而能更好的组织你的代码。

## 参考链接:
1. [Elixir官方文档](https://elixir-lang.org/docs.html)
2. [Phoenix框架创建新项目](https://hexdocs.pm/phoenix/up_and_running.html#content)
3. [《编程Elixir》](https://pragprog.com/book/elixir16/programming-elixir-1-6)