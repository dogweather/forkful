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

## 为什么读取文本文件

读取文本文件是编程中常见的任务之一。无论是读取用户输入、配置文件或者保存的数据，文本文件都是最常见的数据存储形式之一。在Elixir中，读取文本文件也是一项非常简单的任务，而且可以通过一些便捷的函数来完成。

## 如何读取文本文件

让我们来看一个简单的例子：假设我们有一个名为“data.txt”的文本文件，里面包含以下内容：

```
Hello
World
```
为了读取这个文件的内容，我们可以使用Elixir的`File.read!/1`函数，该函数可以读取文件的所有内容并以字符串的形式返回。代码如下：

```Elixir
data = File.read!("data.txt")
```
这将返回一个字符串变量`data`，其中存储着文本文件的内容。我们也可以使用`IO.read/2`函数来读取文件的指定行数：

```Elixir
data = IO.read("data.txt", 1)
```
这将返回文件中第一行的内容“Hello”。我们还可以使用`File.stream!/1`函数来创建一个数据流，然后可以使用`Stream.take/2`函数来读取指定数量的行数：

```Elixir
stream = File.stream!("data.txt")
data = stream |> Stream.take(2)
```
这将返回一个包含文件前两行内容的列表。

## 深入了解文本文件读取

在Elixir中，我们也可以使用`File.open!/2`函数来打开一个文本文件，并使用`IO.binread/2`函数来全文读取文件内容。此外，我们还可以使用`File.stream!/1`函数来创建一个数据流，并结合`Stream.into/2`函数来将数据流转换为一个列表或者数列。

此外，如果我们想要避免在处理大型文本文件时占用过多内存，可以使用Elixir的`File.stream!/2`函数来创建一个懒加载的数据流。这样我们就可以逐行读取文本文件而无需一次性将整个文件加载到内存中。

除了以上的函数，Elixir还提供了许多其他用于读取文本文件的函数和方法，我们可以根据具体的需求选择合适的方式来处理文本文件。

## 查看更多

- [Elixir官方文档](https://hexdocs.pm/elixir/File.html)
- [Elixir File模块文档](https://hexdocs.pm/elixir/File.html#functions)