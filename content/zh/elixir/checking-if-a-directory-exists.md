---
title:    "Elixir: 检查目录是否存在"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

用Elixir编程进行深入探讨是一次令人兴奋的体验。但是，有时候我们可能需要在程序中检查某个目录是否存在。这篇博文将向您介绍为什么需要这样做以及如何实现它。

## 为什么要检查目录的存在

在编写程序时，我们可能需要在特定目录中创建、读取或删除文件。但是，在执行这些操作之前，我们需要先确认该目录是否存在。如果目录不存在，我们的程序将会出现错误。因此，通过检查目录的存在性，我们可以避免潜在的错误。

## 如何实现检查目录的存在

在Elixir中，检查目录的存在有多种方式。下面将使用代码示例来介绍其中两种常用方法：

```Elixir
# 使用Dir模块中的exists?函数来检查一个目录是否存在
Dir.exists?("path/to/directory")

# 使用File模块中的stat!函数来检查一个目录是否存在
File.stat!("path/to/directory")
```

如果目录存在，则以上两个函数都会返回`{:ok, file_info}`，其中`file_info`是有关该目录的信息。如果目录不存在，它们将会抛出一个异常。

## 深入探讨

现在，让我们来深入了解如何实现检查目录的存在。一种常见的做法是结合`Dir.exists?`和`File.stat!`函数来确保目录存在并且可读性良好：

```Elixir
def check_directory(path) do
  if Dir.exists?(path) do
    File.stat!(path)
  else
    IO.puts("Directory does not exist!")
  end
end
```

使用这样的函数，我们可以在需要时检查目录的存在性，并且能够立即捕获错误。

## 参考链接

- [Elixir Dir 模块文档](https://hexdocs.pm/elixir/Dir.html)
- [Elixir File 模块文档](https://hexdocs.pm/elixir/File.html)
- [Elixir 资源大全](https://elixir-resources.readthedocs.io/zh_CN/latest/directory.html#_5)