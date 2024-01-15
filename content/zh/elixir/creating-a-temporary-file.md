---
title:                "创建临时文件"
html_title:           "Elixir: 创建临时文件"
simple_title:         "创建临时文件"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

为什么要创建临时文件

在编程中，有时需要在代码执行过程中临时保存一些数据，这些数据可能不需要长期保存，或者需要在不同的代码块之间共享。创建临时文件就是一种方便的解决方案，可以临时保存数据并在需要的时候读取。

## 如何创建临时文件

可以使用 Elixir 内置的 `File` 模块来创建临时文件。首先，我们需要指定一个文件名（可以使用 `Path` 模块来生成一个随机的文件名），然后使用 `File.write` 函数来写入数据到该文件。

```Elixir
filename = Path.join([Dir.tmpdir(), "tempfile.txt"])
File.write!(filename, "这是一个临时文件的内容")
```

要读取这个临时文件中的内容，可以使用 `File.read` 或者 `File.read!` 函数，并指定文件名作为参数。

```Elixir
File.read!(filename)
# => "这是一个临时文件的内容"
```

当程序执行完毕后，这个临时文件会自动被删除。

## 深入了解创建临时文件

默认情况下，`File.write` 函数会在当前工作目录下创建临时文件，如果需要指定其他路径，可以使用 `:path` 选项来指定。

使用 `File.open` 函数可以返回一个 `File.Stream` 对象，可以在其中写入数据并在必要时手动删除该临时文件。

```Elixir
tempfile = File.open(path, [:binary, :write, :utf8, :delayed_write, path: path])
IO.write(tempfile, "这是一个临时文件的内容")
File.close(tempfile)
```

## 参考链接

- [Elixir File 模块文档](https://hexdocs.pm/elixir/File.html)
- [Elixir Path 模块文档](https://hexdocs.pm/elixir/Path.html)
- [Elixir Dir 模块文档](https://hexdocs.pm/elixir/Dir.html)
- [Elixir File 模块实现源码](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/file.ex)