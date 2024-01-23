---
title:                "开始一个新项目"
date:                  2024-01-20T18:03:35.532031-07:00
model:                 gpt-4-1106-preview
simple_title:         "开始一个新项目"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Getting Started"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
新项目启动意味着从零开始构建一个全新的程序。程序员这么做是为了实现新想法，解决问题，或者尝试学习新技术。

## How to: (如何做：)
要在Gleam中启动一个新项目，确保你已经安装了Gleam。然后，使用以下命令创建一个项目：

```gleam
gleam new my_cool_project
```

运行命令后，你会看到：

```
Your Gleam project "my_cool_project" has been successfully created.
The `my_cool_project` directory contains a new Gleam project you can use to get started.
```

## Deep Dive (深入探讨)
Gleam是一种静态类型的函数编程语言，致力于强大的类型系统和易用的工具链。创建新项目时，Gleam会生成必要的文件结构, 例如`src`, `test`, 和`gleam.toml`等文件。

在历史上，Erlang/OTP是Gleam项目的灵感来源之一，Gleam试图结合Erlang的健壮性与现代语言的类型安全。与直接使用Erlang相比，Gleam提供了一种更现代、更安全的方式来利用Erlang虚拟机（BEAM）。

作为一个新语言，Gleam提供了一些现代化的工具链设计，例如语言服务器和集成测试。这与Elixir等其他BEAM语言共事可以提供不同的体验。

## See Also (另见)
- Gleam官方网站：<https://gleam.run/>
- Gleam语言Github存储库：<https://github.com/gleam-lang/gleam>
- Gleam的Twitter：<https://twitter.com/gleamlang>
- Erlang/OTP：<https://www.erlang.org/>
