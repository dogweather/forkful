---
aliases:
- /zh/elixir/creating-a-temporary-file/
date: 2024-01-20 17:40:18.174686-07:00
description: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u662F\u521B\u5EFA\u4E00\u4E2A\u968F\
  \u7528\u968F\u5220\u7684\u6587\u4EF6\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u4E34\u65F6\u5B58\u50A8\u6570\u636E\uFF0C\u907F\u514D\
  \u51B2\u7A81\u548C\u4E0D\u5FC5\u8981\u7684\u78C1\u76D8\u4F7F\u7528\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:58.883090
model: gpt-4-1106-preview
summary: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6\u662F\u521B\u5EFA\u4E00\u4E2A\u968F\
  \u7528\u968F\u5220\u7684\u6587\u4EF6\u7684\u8FC7\u7A0B\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u6837\u505A\u662F\u4E3A\u4E86\u4E34\u65F6\u5B58\u50A8\u6570\u636E\uFF0C\u907F\u514D\
  \u51B2\u7A81\u548C\u4E0D\u5FC5\u8981\u7684\u78C1\u76D8\u4F7F\u7528\u3002"
title: "\u521B\u5EFA\u4E34\u65F6\u6587\u4EF6"
---

{{< edit_this_page >}}

## What & Why?
创建临时文件是创建一个随用随删的文件的过程。程序员这样做是为了临时存储数据，避免冲突和不必要的磁盘使用。

## How to:
在Elixir中，我们可以用`File`模块来创建和管理临时文件。

```elixir
# 创建临时文件
{:ok, file_path} = File.mktemp()

# 向文件写入一些数据
File.write!(file_path, "Hello Elixir!")

# 读取文件内容并输出
IO.puts(File.read!(file_path))

# 删除临时文件
File.rm!(file_path)
```
输出：
```
Hello Elixir!
```

## Deep Dive
Elixir没有内置专门创建临时文件的功能。我们用了`File.mktemp/0`，一个不常见的函数，因为开发者通常不需要直接处理文件。替代方案可能包括使用数据库或ETS（Erlang Term Storage）来存储临时数据。

底层的Erlang VM确保了文件操作的高效和原子性。`File.mktemp`创建文件时，就地将唯一性和随机性加入文件名，防止冲突。

## See Also
- Elixir File模块文档: https://hexdocs.pm/elixir/File.html
- Erlang :file 文档: http://erlang.org/doc/man/file.html
- 关于ETS的文章: https://elixirschool.com/en/lessons/specifics/ets/
