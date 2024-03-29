---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:05.172678-07:00
description: "\u5728Elixir\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u662F\
  \u5173\u4E8E\u9A8C\u8BC1\u6587\u4EF6\u7CFB\u7EDF\u4E2D\u6307\u5B9A\u8DEF\u5F84\u7684\
  \u76EE\u5F55\u662F\u5426\u5B58\u5728\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u786E\u4FDD\u4ED6\u4EEC\u53EF\u4EE5\u5B89\u5168\u5730\u4ECE\u4E2D\u8BFB\
  \u53D6\u3001\u5199\u5165\u6216\u5BF9\u76EE\u5F55\u6267\u884C\u64CD\u4F5C\uFF0C\u800C\
  \u4E0D\u4F1A\u56E0\u4E3A\u76EE\u5F55\u4E0D\u5B58\u5728\u800C\u9047\u5230\u9519\u8BEF\
  \u3002"
lastmod: '2024-03-13T22:44:47.382587-06:00'
model: gpt-4-0125-preview
summary: "\u5728Elixir\u4E2D\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728\u662F\
  \u5173\u4E8E\u9A8C\u8BC1\u6587\u4EF6\u7CFB\u7EDF\u4E2D\u6307\u5B9A\u8DEF\u5F84\u7684\
  \u76EE\u5F55\u662F\u5426\u5B58\u5728\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\
  \u4E3A\u4E86\u786E\u4FDD\u4ED6\u4EEC\u53EF\u4EE5\u5B89\u5168\u5730\u4ECE\u4E2D\u8BFB\
  \u53D6\u3001\u5199\u5165\u6216\u5BF9\u76EE\u5F55\u6267\u884C\u64CD\u4F5C\uFF0C\u800C\
  \u4E0D\u4F1A\u56E0\u4E3A\u76EE\u5F55\u4E0D\u5B58\u5728\u800C\u9047\u5230\u9519\u8BEF\
  \u3002"
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在Elixir中检查目录是否存在是关于验证文件系统中指定路径的目录是否存在。程序员这样做是为了确保他们可以安全地从中读取、写入或对目录执行操作，而不会因为目录不存在而遇到错误。

## 如何操作：
Elixir的标准库提供了一种直截了当的方式来检查目录是否存在，通过`File`模块。以下是您如何使用它：

```elixir
if File.dir?("path/to/directory") do
  IO.puts "目录存在！"
else
  IO.puts "目录不存在。"
end
```

假设目录不存在的示例输出：
```
目录不存在。
```

对于包括检查目录存在性在内的更高级的文件系统交互，您可能会考虑使用像`FileSystem`这样的第三方库。虽然Elixir的标准功能对于许多情况来说已经足够，但`FileSystem`可以为复杂应用程序提供更细致的控制和反馈。然而，对于检查目录是否存在的基本需求，通常建议坚持使用原生的`File`模块，因为它随时可用且不需要任何外部依赖。
