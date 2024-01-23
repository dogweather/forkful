---
title:                "从字符串解析日期"
date:                  2024-01-20T15:38:02.563990-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
从字符串解析日期，就是把文本格式日期转换成程序能理解的日期对象。程序员这么做，一是为了验证日期数据的真实性，二是方便后续处理，比如排序或比较。

## How to: (如何操作：)
```PowerShell
# 用Get-Date和-culture参数解析特定格式的日期
$dateString = "2023年03月21日"
$parsedDate = Get-Date $dateString -Culture "zh-CN"
Write-Output $parsedDate

# 输出样例：
2023年3月21日 0:00:00
```

```PowerShell
# 用ParseExact方法和自定义格式解析日期
$dateString = "2023-03-21"
$format = "yyyy-MM-dd"
$cultureInfo = [System.Globalization.CultureInfo]::InvariantCulture
$parsedDate = [datetime]::ParseExact($dateString, $format, $cultureInfo)
Write-Output $parsedDate

# 输出样例：
2023年3月21日 0:00:00
```

## Deep Dive (深入解析：)
解析日期始于早期计算需要，为处理不同格式日期数据。在PowerShell中，`Get-Date`命令是主力，通过-culture参数支持各种文化环境。`ParseExact`方法则提供更细粒度控制，需定义准确的格式字符串。

除此之外，还有`TryParse`和`TryParseExact`方法，能在解析失败时不引发异常，而是返回一个布尔值指示成功与否。这些方法在需要验证数据而无需抛出异常时很有用。

通常，不同编程环境都有解析日期的内置函数或方法，但实现细节和性能可能有所不同。PowerShell中的日期解析体现了语言的灵活性和.NET框架的强大功能。

## See Also (另请参阅：)
- [PowerShell官方文档：Get-Date](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date)
- [.NET官方文档：DateTime.ParseExact](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact)
