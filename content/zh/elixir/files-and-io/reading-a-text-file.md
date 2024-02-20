---
date: 2024-01-20 17:54:08.726613-07:00
description: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u8BA9\u7A0B\u5E8F\u80FD\u83B7\u53D6\
  \u548C\u5904\u7406\u5B58\u50A8\u7684\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u5206\u6790\uFF0C\u8F6C\u6362\u6216\u5229\u7528\u8FD9\u4E9B\
  \u4FE1\u606F\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:06.457243
model: gpt-4-1106-preview
summary: "\u8BFB\u53D6\u6587\u672C\u6587\u4EF6\u8BA9\u7A0B\u5E8F\u80FD\u83B7\u53D6\
  \u548C\u5904\u7406\u5B58\u50A8\u7684\u6570\u636E\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u662F\u4E3A\u4E86\u5206\u6790\uFF0C\u8F6C\u6362\u6216\u5229\u7528\u8FD9\u4E9B\
  \u4FE1\u606F\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
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
