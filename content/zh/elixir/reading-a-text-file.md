---
title:                "读取文本文件"
html_title:           "Elixir: 读取文本文件"
simple_title:         "读取文本文件"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 什么是文本文件和它的作用？
文本文件指的是存储文本内容的文件，它通常是以`.txt` 格式结尾。在编程中，我们经常会读取文本文件来获取文件中的信息，比如配置文件、日志文件等。读取文本文件可以帮助我们在程序中使用外部数据，进而提升程序的灵活性和扩展性。

## 如何实现？
在Elixir中，读取文本文件需要通过`File.read!/1` 函数来实现。这个函数会读取指定的文件路径并返回包含文件内容的二进制数据。我们可以通过以下方式来使用这个函数：

```Elixir
file_path = "sample.txt"
file_content = File.read!(file_path)
IO.puts(file_content)
```

上面的代码首先定义了一个文件路径，并通过`File.read!` 函数来读取该文件。然后，我们使用`IO.puts` 函数来打印文件内容。运行这段代码，可以看到对应文件中的文本被打印出来。

## 深入了解
在早期的编程语言中，读取文本文件是一个相当复杂的过程，开发者需要手动处理文件的字节流。但在Elixir中，我们可以通过`File.read!/1` 函数轻松实现文件读取。此外，除了`File.read!/1` 函数外，还有其他一些读取文本文件的方法，比如使用流（Stream）来读取大型文件，或者使用`File.Stream` 模块来一次性读取多个文件的内容。

## 相关链接
- [Elixir文档中的File模块说明][1]
- [关于文本文件的知识][2]

[1]: https://hexdocs.pm/elixir/File.html
[2]: https://www.computerhope.com/issues/ch000969.htm