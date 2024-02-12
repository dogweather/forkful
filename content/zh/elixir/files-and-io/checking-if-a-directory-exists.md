---
title:                "检查目录是否存在"
date:                  2024-02-03T19:07:05.172678-07:00
model:                 gpt-4-0125-preview
simple_title:         "检查目录是否存在"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
