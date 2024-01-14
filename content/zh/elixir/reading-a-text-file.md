---
title:    "Elixir: 读取文本文件"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 为什么

文本文件是电脑中最基本的数据存储形式，它不仅可以存储文字，还可以存储其他格式的信息。在Elixir编程中，读取文本文件是一个常见的操作，它可以让我们轻松地处理和获取文本文件中的信息。因此，了解如何读取文本文件对于想要学习Elixir编程的读者来说是非常重要的一步。

## 如何读取文本文件

在Elixir中，我们可以使用文件模块的read函数来读取文本文件。首先，我们需要使用File.open函数打开我们想要读取的文本文件，然后使用read函数读取文件内容，最后关闭文件。

```Elixir
file = File.open("text.txt")
contents = File.read(file)
File.close(file)

IO.puts(contents)
```

在这个例子中，我们首先使用File.open函数打开名为"test.txt"的文本文件，然后使用read函数读取文件的内容并将其赋值给变量contents，最后通过IO.puts函数来打印出文件的内容。

## 深入了解

在Elixir中，当我们使用read函数来读取文本文件时，它会返回一个二进制字符串。这意味着我们可以使用Elixir的字符串函数来操作这些内容。例如，我们可以使用String.split函数来将字符串按照指定的分隔符进行拆分。

```Elixir
file = File.open("text.csv")
contents = File.read(file)
File.close(file)

rows = String.split(contents, "\n")
IO.inspect(rows)
```

在这个例子中，我们使用read函数读取一个名为"text.csv"的文本文件，然后使用String.split函数将每一行内容按照行分隔符"\n"拆分成一个列表，并使用IO.inspect函数来打印出这个列表。

## 参考链接

- [Elixir文件模块文档](https://hexdocs.pm/elixir/File.html)
- [Elixir字符串模块文档](https://hexdocs.pm/elixir/String.html)
- [Elixir IO模块文档](https://hexdocs.pm/elixir/IO.html)