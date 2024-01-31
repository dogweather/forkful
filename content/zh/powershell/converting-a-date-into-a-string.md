---
title:                "将日期转换为字符串"
date:                  2024-01-20T17:37:28.982484-07:00
model:                 gpt-4-1106-preview
simple_title:         "将日期转换为字符串"

category:             "PowerShell"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/converting-a-date-into-a-string.md"
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
