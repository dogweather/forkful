---
title:                "检查目录是否存在"
html_title:           "Elixir: 检查目录是否存在"
simple_title:         "检查目录是否存在"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 什么 & 为什么? (What & Why?)

检查目录是否存在是程序中的操作，它能帮助你确定特定目录在文件系统中是否已存在。 程序员做这个的原因是为了防止错误，并在尝试访问或修改不存在的目录时提前处理问题。

# 如何操作? (How to) 
在Elixir中，你可以使用File模块中的dir?函数来检查目录是否存在。看下面的例子。

```Elixir
# 引用 File 模块
require File

# 检查目录是否存在
if File.dir?("/path/to/my/directory") do
  IO.puts "The directory exists."
else
  IO.puts "The directory does not exist."
end
```

运行上面的代码，如果路径 `"/path/to/my/directory"` 存在，将会输出 `"The directory exists."`。否则，输出 `"The directory does not exist."`.

# 深潜 (Deep Dive)
在早期的编程中，检查目录是否存在的功能可能不像现在那么普遍。但随着文件系统的使用越来越频繁，这功能变得越来越重要。

对于Elixir，这个功能由内部的 `:filelib` Erlang库实现。Erlang 是 Elixir 的母语言，Elixir 继承了 Erlang的许多功能和库。

此外，你可以使用 `File.exists?/1` 函数来检查文件或目录是否存在。不过，`File.dir?/1` 函数提供了更明确的检查，专门针对目录。

# 另请参阅 (See Also)
欲了解更多相关信息，请访问：

Elixir对文件操作详解： https://learnku.com/elixir/t/2718
Elixir的File模块官方文档： https://hexdocs.pm/elixir/File.html
Erlang的filelib模块官方文档: http://erlang.org/doc/man/filelib.html