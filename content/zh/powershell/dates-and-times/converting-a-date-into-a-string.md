---
date: 2024-01-20 17:37:28.982484-07:00
description: "\u65E5\u671F\u8F6C\u6362\u6210\u5B57\u7B26\u4E32\u5C31\u662F\u5C06\u65E5\
  \u671F\u683C\u5F0F\u53D8\u4E3A\u53EF\u9605\u8BFB\u7684\u6587\u672C\u5F62\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8F93\u51FA\u663E\u793A\uFF0C\
  \u50A8\u5B58\u6216\u8005\u662F\u6570\u636E\u5904\u7406\u7684\u4FBF\u5229\u6027\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.027060-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u671F\u8F6C\u6362\u6210\u5B57\u7B26\u4E32\u5C31\u662F\u5C06\u65E5\
  \u671F\u683C\u5F0F\u53D8\u4E3A\u53EF\u9605\u8BFB\u7684\u6587\u672C\u5F62\u5F0F\u3002\
  \u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u8F93\u51FA\u663E\u793A\uFF0C\
  \u50A8\u5B58\u6216\u8005\u662F\u6570\u636E\u5904\u7406\u7684\u4FBF\u5229\u6027\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
日期转换成字符串就是将日期格式变为可阅读的文本形式。程序员这样做是为了输出显示，储存或者是数据处理的便利性。

## How to: (如何操作：)
```PowerShell
# 简单转换
Get-Date | Out-String
# 指定格式
Get-Date -Format "yyyy-MM-dd"
# 示例输出
2023-04-01
```

```PowerShell
# 自定义格式
$date = Get-Date
$date.ToString("MMMM dd, yyyy")
# 示例输出
April 01, 2023
```

```PowerShell
# 使用文化特定格式
(Get-Culture).DateTimeFormat.ShortDatePattern
# 示例输出
MM/dd/yyyy
```

## Deep Dive (深入探索)
历史上，日期格式变化多端，不同文化有各自标准。PowerShell 使用 .NET 的 `DateTime` 类实现日期和字符串之间的转换。除了 `ToString()` 和 `Format` 参数，你还可以用 `[datetime]::Parse()` 和 `[datetime]::ParseExact()` 来从字符串解析出日期。

在 PowerShell 的世界里，日期往往用 `DateTime` 对象表示，它包含了丰富的方法和属性。利用 `.ToString()` 方法时，我们能够按需求自定义输出格式；而使用 `Get-Date -Format`，则允许你直接输出指定格式的字符串。不要忘记，实现这一功能的是背后强大的 .NET 框架。

为何视情况而定选择不同方式？有时为了兼容性或是为了满足国际化需求，使用 `Get-Culture` 来获取当前文化的日期和时间格式可能更为合适。

## See Also (推荐阅读)
- [官方 PowerShell 文档](https://docs.microsoft.com/powershell/)
- [.NET DateTime 文档](https://docs.microsoft.com/dotnet/api/system.datetime)
