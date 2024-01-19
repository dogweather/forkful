---
title:                "将日期转换为字符串"
html_title:           "Bash: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么？
日期转字符串是将日期对象变成可阅读的文字形式的过程。程序员这么做是为了能轻松地在显示设备上展示日期，或者以文字形式存储和传输数据。

## 怎么做：
在 PowerShell 中，我们可以使用 `Get-Date` cmdlet 和 `-Format` 参数进行日期转字符串操作。
```PowerShell
# 获取当前日期并转化为字符串
$日期 = Get-Date -Format "yyyy-MM-dd"

# 显示结果
$日期
```
在上述例子中，你可能会得到如下输出：
```PowerShell
2022-04-29
```
## 深入研究
在基于 Unix 的操作系统（比如：Linux）问世之初，日期与时间的表示包含了很多复杂性。为了简化处理，开发者把日期和时间转化为字符串以方便处理和展示。

在 PowerShell 里，除了 `Get-Date -Format` 之外，我们还可以使用 `.ToString()` 方法进行日期转字符串操作。这两者的主要区别在于，使用 `Get-Date -Format` 可以支持 Unix 风格的日期和时间字符串格式，而 `.ToString()` 方法则更倾向于 .NET 原生的日期和时间字符串格式。

## 参考文档：
- PowerShell 的 [`Get-Date`](https://docs.microsoft.com/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1) cmdlet 官方文档.
- `.NET` 的 [`DateTime.ToString`](https://docs.microsoft.com/dotnet/api/system.datetime.tostring?view=net-5.0) 方法官方文档.
- 关于更多日期和时间字符串格式的讨论，你可以参考 [ISO 8601](https://en.wikipedia.org/wiki/ISO_8601) 标准.