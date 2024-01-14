---
title:                "Elixir: 读取文本文件。"
simple_title:         "读取文本文件。"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么要阅读文本文件？

在编程中，阅读文本文件是非常常见的任务，特别是在处理大量数据时。Elixir编程语言提供了许多方法来读取和处理文本文件，使得这个任务变得更加简单和高效。

## 如何实现

首先，我们需要使用Elixir的File模块来打开和读取文本文件。以下是一个简单的示例：

```
Elixir
file = File.open("sample.txt", [:read])
```

这将打开名为 “sample.txt” 的文本文件，并将其存储在变量 “file” 中。接下来，我们可以使用 `IO.read(file, :line)` 来逐行读取文件的内容。

```
Elixir
IO.read(file, :line)
```

这将打印出文件的第一行内容。我们也可以使用 `IO.binread(file, 10)` 来读取指定字节数的内容，这在需要处理二进制文件时非常有用。

除了逐行读取，Elixir还提供了许多其他方法来读取和处理文本文件，例如使用 `File.stream!(file)` 来创建一个流对象，可以逐行读取文件的内容。更多的例子和代码可以在Elixir官方文档中找到。

## 深入探讨

在Elixir中，文本文件是以UTF-8格式存储的，因此我们可以使用 `String.to_char_list(content)` 将其内容转换为字符列表，并使用字符串函数来处理它们。

另外，Elixir还提供了 `File.read!(file)` 方法来一次性读取整个文件的内容，并将其存储为一个字符串。这在需要处理较小文本文件时非常有用。

此外，我们还可以使用File模块的许多其他方法来处理文本文件，例如 `File.write(file, "Hello, World!")` 可以将字符串内容“Hello, World!”写入文件中。

## 参考链接

* [Elixir File模块文档](https://hexdocs.pm/elixir/File.html)
* [Elixir String模块文档](https://hexdocs.pm/elixir/String.html)
* [Elixir IO模块文档](https://hexdocs.pm/elixir/IO.html)
* [Elixir文本文件读写示例代码](https://elixirschool.com/cn/lessons/basics/io/#%E8%AF%BB%E5%B1%8F%E8%94%BD)