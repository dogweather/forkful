---
title:                "检查目录是否存在"
date:                  2024-01-20T14:56:03.124187-07:00
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"

category:             "Elixir"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？

检查目录是否存在是文件系统操作，核实给定路径的文件夹是否真的在那儿。程序员这样做，是为了防止错误，确保在尝试读写文件前，目录已就绪。

## How to: 怎么做：

```elixir
# 检查目录是否存在：
File.dir?("path/to/directory")

# 样例输出：
true  # 如果目录存在
false # 如果目录不存在
```

## Deep Dive 深入探索

在 Elixir 语言中， File 模块对文件系统进行聚合。历史上，我们会用操作系统命令或其他编程语言进行这项检查。不同于其它语言的复杂性，Elixir 提供了简单的方法。替代方案包括使用 `:filelib` Erlang 库。Elixir中的 `File.dir?/1` 函数实际上用的是 Erlang 的 `:filelib.is_dir/1` 函数。

## See Also 查看更多

- Elixir 官方文档在这里: [File module](https://hexdocs.pm/elixir/File.html)
- Erlang 的 `filelib` 模块: [filelib](http://erlang.org/doc/man/filelib.html)
