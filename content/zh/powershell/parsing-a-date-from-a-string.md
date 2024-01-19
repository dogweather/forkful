---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么以及为什么？

日期解析是将文本形式的日期转换为程序可以处理的日期对象的过程。我们之所以需要这样做，是因为在处理日期数据时，原始数据通常以文本形式提供，我们需要将它转换为日期对象以便进行计算和比较。

## 演示如何：

在 PowerShell 中解析日期字符串非常简单直接，我们可以直接使用 `Get-Date` cmdlet，或者 `[datetime]` 类型转换。首先，让我们从基本的开始：

```PowerShell
# 使用 Get-Date
$dateString = "2021-11-09 20:30"
$date = Get-Date $dateString
$date
```

运行此代码后，输出将显示如下：

```PowerShell
2021年11月9日 20:30:00
```

现在我们试试看 `[datetime]` 类型转换：

```PowerShell
# 使用 [datetime] 转换
$date = [datetime] "2021/11/09"
$date
```

运行此代码后，输出将显示如下：

```PowerShell
2021年11月9日 0:00:00
```

## 深度剖析：

日期的解析在计算机的历史中早已存在，但在 PowerShell 存在的`Get-Date`和`[datetime]`之前，往往需要复杂的步骤和逻辑。PowerShell 的这些方法为我们提供了简洁易用且被广泛接受的解决方案。

除了上述方法，你还可以使用 .NET Framework 的 `DateTime.Parse`或`DateTime.TryParse` 方法进行更复杂或自定义的日期解析。一些刊在MSDN和相关网站上的文档和教程详细描述了如何使用这些方法。

## 参见：

1. [PowerShell 官方文档](https://docs.microsoft.com/zh-cn/powershell/)
2. [Get-Date 官方文档](https://docs.microsoft.com/zh-cn/powershell/module/Microsoft.PowerShell.Utility/Get-Date)
3. [DateTime.Parse 官方文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime.parse) 
4. [DateTime.TryParse 官方文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime.tryparse)