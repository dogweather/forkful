---
title:                "阅读文本文件"
date:                  2024-01-20T17:54:08.726613-07:00
model:                 gpt-4-1106-preview
simple_title:         "阅读文本文件"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? / 什么和为什么?
读取文本文件让程序能获取和处理存储的数据。程序员这样做是为了分析，转换或利用这些信息。

## How to / 怎么做
以下是Elixir代码示例：

```elixir
# 载入文件读取函数
File.read("hello.txt")
# 如果文件存在，返回 {:ok, "文件内容"}
# 如果文件不存在，返回 {:error, :enoent}
```

```elixir
# 逐行读取
File.stream!("hello.txt") 
|> Enum.each(fn line -> 
   IO.puts(line)
end)
# 输出文件的每一行
```

## Deep Dive / 深入探索
在Elixir 1.0发布时，`File.read/1`和`File.stream!/1`就存在了，让读取文件变得简单。选择读取整个文件或逐行读取取决于文件大小和需求；小文件通常一次性读取，大文件建议逐行处理。

`File.stream!/1` 返回一个流（Stream），这是一种懒序列，逐个产出文件中的行，有效管理内存消耗。

替代方案如使用`:file.open`和`:file.read_line`等低级Erlang函数提供了更多控制，但通常不需要。

## See Also / 另请参阅
- Elixir官方文档: [https://elixir-lang.org/docs.html](https://elixir-lang.org/docs.html)
- Erlang的`:file`模块文档: [http://erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)
- Elixir School的文件操作教程: [https://elixirschool.com/en/lessons/basics/collections/](https://elixirschool.com/en/lessons/basics/collections/)
- Elixir Forum的讨论和问题: [https://elixirforum.com/](https://elixirforum.com/)