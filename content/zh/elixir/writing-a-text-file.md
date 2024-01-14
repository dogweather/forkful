---
title:                "Elixir: 编写一个文本文件"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

##为什么

为什么有必要创建文本文件？其实，文本文件是日常编程工作中必不可少的一部分。它们可以用来存储程序代码、记录编程过程中的重要信息，以及作为程序输出的一种方式。

##如何

如果想要在Elixir中创建文本文件，只需要使用File模块的write函数。首先，我们需要提供要写入的文件名，然后是要写入的内容。代码如下所示：

```Elixir
File.write("new_file.txt", "这是一个文本文件的内容")
```

执行上述代码后，会在当前文件夹下创建一个名为"new_file.txt"的文本文件，并将内容写入其中。如果想要在已有的文本文件中添加内容，可以使用File模块的append函数，代码如下：

```Elixir
File.append("existing_file.txt", "这是新添加的内容")
```

##深入了解

在Elixir中，文本文件是通过二进制数据来表示的。因此，我们可以使用二进制数据处理函数来对文本文件进行操作。比如，可以使用File模块的read函数来读取文本文件的内容，代码如下：

```Elixir
File.read("existing_file.txt") #=> {:ok, "文本文件的内容"}
```

另外，Elixir还提供了更简便的函数来创建文本文件，比如IO模块的puts函数，可以直接将内容输出到标准输出流，代码如下：

```Elixir
IO.puts("这是输出到文本文件的内容")
```

##看看这些

- [Elixir文档](https://hexdocs.pm/elixir/1.11.2/)

- [Elixir字符串操作指南](https://elixirschool.com/zh-cn/lessons/basics/string-operations/)

- [用Elixir创建命令行脚本](https://qiita.com/takasehideki/items/e2d2f76d2d5e54ab4ec3)