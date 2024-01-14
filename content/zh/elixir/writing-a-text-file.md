---
title:    "Elixir: 编写文本文件"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

为什么：每个程序员都知道，文本文件是编程中不可或缺的一部分。它们是存储代码，文档和配置信息的基本方式。因此，学习如何编写文本文件对于在计算机科学领域工作的人来说是必要的。

如何：首先，我们可以使用Elixir标准库中的File模块来创建和打开文本文件。我们可以使用File.open/2函数来打开文件，第一个参数是文件名，第二个参数是打开的模式。例如，要创建一个名为“example.txt”的文件并写入一些文本，我们可以按照以下方式编写代码：

```Elixir
File.open("example.txt", [:write], fn(file) ->
  IO.write(file, "Hello World!")
end)
```

这将在当前目录下创建一个“example.txt”文件，并将“Hello World!”文本写入该文件。我们还可以使用File.write/2函数来直接写入文本，如下所示：

```Elixir
File.write("example.txt", "Hello World!")
```

深入探讨：除了创建和写入文本文件，我们还可以使用File.read/1函数来读取现有的文本文件。该函数接受一个文件名作为参数，并返回文件的内容。例如，我们可以按照以下方式读取“example.txt”文件的内容：

```Elixir
content = File.read("example.txt")
```

然后，我们可以使用IO.puts/1函数将文件内容打印到控制台上：

```Elixir
IO.puts(content)
```

另外，我们还可以使用File.close/1函数来关闭打开的文件，并释放资源。这对于处理大量的文件操作是非常重要的。完整的代码示例如下：

```Elixir
File.open("example.txt", [:write], fn(file) ->
  IO.write(file, "Hello World!")
end)

content = File.read("example.txt")
IO.puts(content)

File.close("example.txt")
```

另一件值得注意的事情是在处理文本文件时，我们应该小心处理编码格式。Elixir中的File模块可以使用Unicode编码，因此我们可以使用String.to_charlist/1函数来正确处理文件中的Unicode字符。

另外，我们也可以使用Erlang标准库中的File.IO模块来实现更多高级的文件操作，如文件复制，重命名，删除等。这可以通过在文件名前添加“:file.”前缀来调用，例如“File.IO.copy/2”。

见下文： - [Elixir官方文档: File模块](https://hexdocs.pm/elixir/File.html) - [Elixir官方文档: String模块](https://hexdocs.pm/elixir/String.html) - [Elixir官方文档: Erlang标准库: File模块](https://erlang.org/doc/man/file.html)