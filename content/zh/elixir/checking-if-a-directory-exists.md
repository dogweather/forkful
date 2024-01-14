---
title:    "Elixir: 检查目录是否存在"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 为什么

你是否曾经在编程过程中需要确认某个目录是否存在？可能有时候你会创建一个新的文件夹，但又不确定它是否已经存在。或者你想要在特定的路径下创建一个新文件，但是需要先确认路径是否存在。无论什么情况，确认目录是否存在都是一个重要的步骤，它可以帮助你避免一些不必要的错误。

## 如何

使用Elixir编程语言很容易检查目录是否存在。首先，在你的代码中导入 `File` 模块。然后使用 `File.dir?/1` 函数并传入目录的路径作为参数。下面是一个示例代码：

```elixir
import File
exists = File.dir?("/Users/username/my_folder")
IO.puts "目录是否存在：#{exists}"
```

如果目录存在，`File.dir?/1` 函数会返回 `true`，如果不存在则会返回 `false`。在上面的示例代码中，我们使用 `IO.puts` 函数来打印结果并验证目录是否存在。

## 深入探讨

在Elixir中，使用 `File.dir?/1` 函数只能检查目录是否存在，但不能判断它是否为空。如果你想要删除一个目录，应该先使用 `File.rm_rf/1` 函数来删除目录下的所有文件和子目录，然后再使用 `File.rmdir/1` 函数来删除空目录。

此外，当使用 `File.dir?/1` 函数时，需要注意传入的路径是否正确。如果路径不存在，函数会报错。因此，在使用此函数之前，最好先通过 `File.exists?/1` 函数来确认路径是否存在。

## 参考链接

如果你想要深入学习Elixir中关于文件操作的知识，可以参考以下链接：

- Elixir官方文档：https://elixir-lang.org/getting-started/file-operations.html
- Elixir学习指南：https://elixirschool.com/zh-cn/lessons/basics/file-operations/

## 参见

- [Elixir官方文档](https://elixir-lang.org/getting-started/file-operations.html)
- [Elixir学习指南](https://elixirschool.com/zh-cn/lessons/basics/file-operations/)