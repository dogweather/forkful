---
title:                "开始新项目"
html_title:           "Lua: 开始新项目"
simple_title:         "开始新项目"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## 什么和为什么？
启动新项目就是随着一个新的编程想法来开始一个全新的开发旅程。程序员这样做是为了实现他们的独特想法，解决特定问题或起步创新技术。

## 如何做：
在Gleam中创建一个新项目非常简单，可以通过下面几步：
```Gleam
// 安装Gleam
$ rebar3 update
$ rebar3 new gleam_lib your_project

// 进入到你的项目
$ cd your_project

// 编译项目
$ rebar3 eunit
```
‘your_project’ 是你起的工程名。 'rebar3 eunit' 会编译你的项目并运行任何EUnit测试。

## 深入探讨
在Gleam诞生之前，我们通常使用如Python、Java或JavaScript等其他语言进行编程。但Gleam赋予了Erlang强类型的能力，即使他还是一个相对年轻的语言。启动一个新项目的其他方式可能包括使用特定的IDE，或者待更多的自动化工具完成。

Gleam背后的实现由Rust编写，它利用了Rust的高级类型系统和模式匹配功能，以支持Erlang的强并发和故障恢复能力。这就是为什么在Gleam中，你会发现它与Erlang有许多相似之处。

## 另请参见
- Gleam 文档: https://gleam.run/book/getting-started/
- Gleam 代码基地: https://github.com/gleam-lang/gleam
- Gleam 社区: https://elixirforum.com/c/gleam
- Erlang: https://www.erlang.org/