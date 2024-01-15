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

# 为什么

在编写程序时，有时需要检查一个目录是否存在。这样做可以帮助我们避免出现错误，例如在尝试访问不存在的目录时程序会崩溃。

# 如何实现

在Elixir中，我们可以使用`File.exists?/1`函数来检查目录是否存在。下面是一个简单的例子：

```Elixir
if File.exists?("path/to/directory") do
  IO.puts("目录存在")
else
  IO.puts("目录不存在")
end
```

以上代码会先检查给定的路径是否存在，如果存在则会输出“目录存在”，否则会输出“目录不存在”。

# 深入了解

要想更详细地检查目录是否存在，我们可以使用`File.stat/1`函数。该函数会返回一个包含文件或目录属性的结构体，我们可以通过其中的`:type`字段来查看目录是否存在。下面是一个示例代码：

```Elixir
file = File.stat("path/to/directory")

if file.type == :directory do
  IO.puts("目录存在")
else
  IO.puts("目录不存在")
end
```

通过这种方法，我们可以获得更详细的信息，例如目录的创建时间、大小等等。

# 查看也可以

如果你想了解更多关于Elixir中检查目录存在的方法，可以参考官方文档或下面的链接。

[官方文档](https://hexdocs.pm/elixir/File.html#exists?/1)

[Elixir Getting Started](https://elixir-lang.org/getting-started/introduction.html)

[Deep Dive Into Elixir](https://www.freecodecamp.org/news/elixir-deep-dive-into-the-mind-of-elixir/)