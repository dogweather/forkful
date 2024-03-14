---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:27:39.605313-07:00
description: "\u5728Elixir\u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u5BF9\u5F00\u53D1\
  \u8005\u800C\u8A00\u662F\u4E00\u9879\u57FA\u672C\u6280\u80FD\uFF0C\u5B83\u5141\u8BB8\
  \u8FDB\u884C\u6570\u636E\u6301\u4E45\u5316\u3001\u8BB0\u5F55\u65E5\u5FD7\u6216\u5BFC\
  \u51FA\u4EBA\u7C7B\u53EF\u8BFB\u7684\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u5B9E\u73B0\
  \u8FD9\u4E00\u70B9\u662F\u4E3A\u4E86\u4FDD\u5B58\u5E94\u7528\u72B6\u6001\u3001\u8C03\
  \u8BD5\u4FE1\u606F\u3001\u914D\u7F6E\u6216\u4EFB\u4F55\u9700\u8981\u5728\u7CFB\u7EDF\
  \u95F4\u4EA4\u6362\u7684\u6570\u636E\uFF0C\u800C\u4E14\u503E\u5411\u4E8E\u4F7F\u7528\
  \u50CF\u6587\u672C\u8FD9\u6837\u7684\u666E\u904D\u683C\u5F0F\u3002"
lastmod: '2024-03-13T22:44:47.387082-06:00'
model: gpt-4-0125-preview
summary: "\u5728Elixir\u4E2D\u5199\u5165\u6587\u672C\u6587\u4EF6\u5BF9\u5F00\u53D1\
  \u8005\u800C\u8A00\u662F\u4E00\u9879\u57FA\u672C\u6280\u80FD\uFF0C\u5B83\u5141\u8BB8\
  \u8FDB\u884C\u6570\u636E\u6301\u4E45\u5316\u3001\u8BB0\u5F55\u65E5\u5FD7\u6216\u5BFC\
  \u51FA\u4EBA\u7C7B\u53EF\u8BFB\u7684\u5185\u5BB9\u3002\u7A0B\u5E8F\u5458\u5B9E\u73B0\
  \u8FD9\u4E00\u70B9\u662F\u4E3A\u4E86\u4FDD\u5B58\u5E94\u7528\u72B6\u6001\u3001\u8C03\
  \u8BD5\u4FE1\u606F\u3001\u914D\u7F6E\u6216\u4EFB\u4F55\u9700\u8981\u5728\u7CFB\u7EDF\
  \u95F4\u4EA4\u6362\u7684\u6570\u636E\uFF0C\u800C\u4E14\u503E\u5411\u4E8E\u4F7F\u7528\
  \u50CF\u6587\u672C\u8FD9\u6837\u7684\u666E\u904D\u683C\u5F0F\u3002"
title: "\u7F16\u5199\u6587\u672C\u6587\u4EF6"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在Elixir中写入文本文件对开发者而言是一项基本技能，它允许进行数据持久化、记录日志或导出人类可读的内容。程序员实现这一点是为了保存应用状态、调试信息、配置或任何需要在系统间交换的数据，而且倾向于使用像文本这样的普遍格式。

## 如何操作：

Elixir通过内置模块使文件处理变得简单。写入文件的主要方式是使用`File.write/2`或`File.write!/2`函数，前者返回一个`:ok`或`:error`元组，后者在失败时引发错误。

以下是一个简单的例子：

```elixir
# 写入文件，简单消息
File.write("hello.txt", "Hello, World!")

# 当你运行代码时，它会创建"hello.txt"并写入"Hello, World!"作为内容
```

要向文件追加内容，你应使用`File.open/3`并带上`[:write, :append]`选项，然后使用`IO.binwrite/2`来追加内容：

```elixir
# 追加到文件
{:ok, file} = File.open("hello.txt", [:write, :append])
IO.binwrite(file, "\nLet's add another line.")
File.close(file)

# 现在"hello.txt"包括了第二行"Let's add another line."
```

如果你正在处理大量数据或需要对写入过程有更多控制，你可能会使用`Stream`模块来懒加载地写入数据到文件：

```elixir
# 懒加载地写入大量数据集
stream_data = Stream.iterate(0, &(&1 + 1))
            |> Stream.map(&("Number: #{&1}\n"))
            |> Stream.take(10)

File.open!("numbers.txt", [:write], fn file ->
  Enum.each(stream_data, fn line ->
    IO.write(file, line)
  end)
end)

# 这将创建"numbers.txt"，写入数字0到9，每个数字占一行。
```

对于需要更复杂文件处理的项目，你可能会查看第三方库如`CSV`，它为CSV文件操作提供了专门的功能，但请记住，对于许多目的而言，Elixir的内置功能已经绰绰有余。
