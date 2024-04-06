---
date: 2024-01-20 17:37:28.982484-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5386\u53F2\u4E0A\uFF0C\u65E5\
  \u671F\u683C\u5F0F\u53D8\u5316\u591A\u7AEF\uFF0C\u4E0D\u540C\u6587\u5316\u6709\u5404\
  \u81EA\u6807\u51C6\u3002PowerShell \u4F7F\u7528 .NET \u7684 `DateTime` \u7C7B\u5B9E\
  \u73B0\u65E5\u671F\u548C\u5B57\u7B26\u4E32\u4E4B\u95F4\u7684\u8F6C\u6362\u3002\u9664\
  \u4E86 `ToString()` \u548C `Format` \u53C2\u6570\uFF0C\u4F60\u8FD8\u53EF\u4EE5\u7528\
  \ `[datetime]::Parse()` \u548C\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.182929-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u5386\u53F2\u4E0A\uFF0C\u65E5\u671F\u683C\
  \u5F0F\u53D8\u5316\u591A\u7AEF\uFF0C\u4E0D\u540C\u6587\u5316\u6709\u5404\u81EA\u6807\
  \u51C6\u3002PowerShell \u4F7F\u7528 .NET \u7684 `DateTime` \u7C7B\u5B9E\u73B0\u65E5\
  \u671F\u548C\u5B57\u7B26\u4E32\u4E4B\u95F4\u7684\u8F6C\u6362\u3002\u9664\u4E86 `ToString()`\
  \ \u548C `Format` \u53C2\u6570\uFF0C\u4F60\u8FD8\u53EF\u4EE5\u7528 `[datetime]::Parse()`\
  \ \u548C `[datetime]::ParseExact()` \u6765\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u51FA\
  \u65E5\u671F\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

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
