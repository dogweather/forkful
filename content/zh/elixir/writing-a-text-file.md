---
title:                "编写文本文件"
date:                  2024-01-19
html_title:           "Arduino: 编写文本文件"
simple_title:         "编写文本文件"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (什么以及为什么？)

写入文本文件就是将字符串数据保存到文件系统中的标凑操作。程序员这么做是为了数据持久化，日志记录，或给其他程序传递信息。

## How to: (如何操作：)

```Elixir
# 写入字符串到文件
File.write!("hello.txt", "你好，世界！")

# 检查文件内容
IO.puts(File.read!("hello.txt"))
```
输出:
```
你好，世界！
```

## Deep Dive (深入探讨)

写入文本文件是自编程诞生以来就存在的需求。Elixir中用`File`模块和Erlang虚拟机的能力提供这一功能。与文件流操作——如`IO.stream`——相比，`File.write!`简单明了，适用于一次写入场景。写入大文件时，考虑使用流式写入，这对内存更友好。

## See Also (另请参阅)

- Elixir 文件操作官方文档: [File Module](https://hexdocs.pm/elixir/File.html)
- Erlang 文件I/O: [Erlang :file](http://erlang.org/doc/man/file.html)
- Elixir学习资源: [Elixir School](https://elixirschool.com/en/)
