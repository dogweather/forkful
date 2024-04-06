---
date: 2024-01-20 17:54:08.726613-07:00
description: "How to / \u600E\u4E48\u505A \u5728Elixir 1.0\u53D1\u5E03\u65F6\uFF0C\
  `File.read/1`\u548C`File.stream!/1`\u5C31\u5B58\u5728\u4E86\uFF0C\u8BA9\u8BFB\u53D6\
  \u6587\u4EF6\u53D8\u5F97\u7B80\u5355\u3002\u9009\u62E9\u8BFB\u53D6\u6574\u4E2A\u6587\
  \u4EF6\u6216\u9010\u884C\u8BFB\u53D6\u53D6\u51B3\u4E8E\u6587\u4EF6\u5927\u5C0F\u548C\
  \u9700\u6C42\uFF1B\u5C0F\u6587\u4EF6\u901A\u5E38\u4E00\u6B21\u6027\u8BFB\u53D6\uFF0C\
  \u5927\u6587\u4EF6\u5EFA\u8BAE\u9010\u884C\u5904\u7406\u3002 `File.stream!/1`\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:51:00.598012-06:00'
model: gpt-4-1106-preview
summary: "`File.stream!/1` \u8FD4\u56DE\u4E00\u4E2A\u6D41\uFF08Stream\uFF09\uFF0C\u8FD9\
  \u662F\u4E00\u79CD\u61D2\u5E8F\u5217\uFF0C\u9010\u4E2A\u4EA7\u51FA\u6587\u4EF6\u4E2D\
  \u7684\u884C\uFF0C\u6709\u6548\u7BA1\u7406\u5185\u5B58\u6D88\u8017\u3002"
title: "\u9605\u8BFB\u6587\u672C\u6587\u4EF6"
weight: 22
---

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
