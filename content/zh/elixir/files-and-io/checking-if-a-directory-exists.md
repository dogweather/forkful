---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:05.172678-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir\u7684\u6807\u51C6\u5E93\u63D0\u4F9B\
  \u4E86\u4E00\u79CD\u76F4\u622A\u4E86\u5F53\u7684\u65B9\u5F0F\u6765\u68C0\u67E5\u76EE\
  \u5F55\u662F\u5426\u5B58\u5728\uFF0C\u901A\u8FC7`File`\u6A21\u5757\u3002\u4EE5\u4E0B\
  \u662F\u60A8\u5982\u4F55\u4F7F\u7528\u5B83\uFF1A."
lastmod: '2024-04-05T22:38:46.548226-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A Elixir\u7684\u6807\u51C6\u5E93\u63D0\u4F9B\
  \u4E86\u4E00\u79CD\u76F4\u622A\u4E86\u5F53\u7684\u65B9\u5F0F\u6765\u68C0\u67E5\u76EE\
  \u5F55\u662F\u5426\u5B58\u5728\uFF0C\u901A\u8FC7`File`\u6A21\u5757\u3002\u4EE5\u4E0B\
  \u662F\u60A8\u5982\u4F55\u4F7F\u7528\u5B83\uFF1A."
title: "\u68C0\u67E5\u76EE\u5F55\u662F\u5426\u5B58\u5728"
weight: 20
---

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
