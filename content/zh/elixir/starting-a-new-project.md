---
title:                "开始一个新项目"
date:                  2024-01-20T18:03:08.445990-07:00
model:                 gpt-4-1106-preview
simple_title:         "开始一个新项目"

category:             "Elixir"
tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
新建项目，就是从零开始创建程序。程序员新建项目来实现想法，解决问题。

## How to: (如何操作：)
在Elixir中新建项目很简单。使用Mix工具，一键生成项目结构。

```elixir
mix new my_app
```

执行后的输出：
```
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/my_app.ex
* creating test
* creating test/test_helper.exs
* creating test/my_app_test.exs

Your Mix project was created successfully.
You can use "mix" to compile it, test it, and more.

Navigate into your new project directory and run "iex -S mix" to interact with your project.
```

## Deep Dive (深入探索):
Elixir语言由José Valim开发，属Erlang虚拟机（BEAM）家族。利用Elixir开始新项目，便可享受Erlang的并发优势，同时有更现代的语法。

现代的Elixir项目用Mix构建。早期，没有这么方便。之前的语言使用不同工具，如Java的Maven，Ruby的Bundler。Elixir的Mix工具集成了创建、编译、测试等多种功能。

选择Elixir开始新项目意味着选择了高效开发、容错性高的应用程序。它非常适合高并发场景。

## See Also (另请参阅):
- [Elixir官方入门指南](https://elixir-lang.org/getting-started/introduction.html)
- [Mix & OTP指南](https://elixir-lang.org/getting-started/mix-otp/introduction-to-mix.html)
- [Elixir School](https://elixirschool.com/)
