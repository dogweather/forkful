---
title:                "Elixir: 检查目录是否存在"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## 为什么

在编程中，我们经常需要在代码中检查某个文件夹是否存在。这可以帮助我们确定文件夹是否已经创建，以便我们可以在后续的代码中使用它。

## 如何操作

检查文件夹是否存在的最简单方法是使用 `File.dir?/1` 函数，该函数接受一个文件夹的路径作为参数，并返回一个布尔值来指示该文件夹是否存在。下面是一个简单的示例代码：

```elixir
if File.dir?("/path/to/directory") do
  IO.puts "Directory exists"
else
  IO.puts "Directory does not exist"
end
```

如果您想要检查文件夹是否存在并且具有特定权限，可以使用 `File.dir?(path, mode)` 函数。该函数接受一个路径和权限参数，并返回一个布尔值来指示是否具有指定权限的文件夹存在。下面是一个示例代码：

```elixir
if File.dir?("/path/to/directory", :read) do
  IO.puts "Directory exists and has read permission"
else
  IO.puts "Directory does not exist or does not have read permission"
end
```

## 深入了解

要更深入地了解如何在Elixir中检查文件夹的存在，您可以研究 `File.dir?/1` 函数的源代码。您还可以了解Elixir中的其他文件和文件夹相关函数，例如 `File.cwd/0`用于获取当前工作目录，`File.mkdir/1`用于创建文件夹等。

## 参考链接

- Elixir官方文档：https://elixir-lang.org/getting-started/introduction.html
- Elixir文件操作指南：https://elixir-lang.org/getting-started/file-operations.html
- Elixir中的File模块：https://hexdocs.pm/elixir/File.html
- Elixir中的Path模块：https://hexdocs.pm/elixir/Path.html

## 请参阅