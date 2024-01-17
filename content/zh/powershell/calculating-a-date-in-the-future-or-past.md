---
title:                "在计算机编程中，“计算未来或过去的日期”。"
html_title:           "PowerShell: 在计算机编程中，“计算未来或过去的日期”。"
simple_title:         "在计算机编程中，“计算未来或过去的日期”。"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么和为什么？

计算未来或过去日期是指用编程语言计算某个时间的具体日期。程序员经常会使用这个功能来轻松处理日期相关的任务，例如计算未来事件的日期或者计算某个事件已经过去的天数。

## 如何：

### 计算未来日期：

`PowerShell AddDays`命令可以让我们根据当前日期计算若干天后的日期。下面是一个简单的例子：

```PowerShell
$date = Get-Date
$date.AddDays(3)
```

输出：

```PowerShell
星期五, 六月 21, 2019 10:26:46 AM
```

### 计算过去日期：

`PowerShell Subtract` 命令可以用来计算当前日期之前的日期。下面是一个例子：

```PowerShell
$date = Get-Date
$date.Subtract([TimeSpan]::FromDays(15))
```

输出：

```PowerShell
星期二, 六月 04, 2019 10:29:10 AM
```

## 深入了解：

### 历史背景：

在过去，人们使用手动计算来算出未来或过去的日期。但是随着计算机的出现，编程语言的发展，程序员可以通过简单的代码来实现这个功能。

### 替代方法：

除了使用PowerShell命令，程序员也可以使用其他编程语言来计算日期，例如Python的datetime库、C#的DateTime类等。

### 实现细节：

PowerShell的Get-Date命令可以获取当前日期，AddDays命令和Subtract命令可以用来进行日期的计算。需要注意的是，日期计算也受系统时区和语言设置的影响。

## 参考链接：

- [Get-Date Command (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date)
- [Math in PowerShell (Microsoft Docs)](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_arithmetic_operators?view=powershell-6)
- [Python datetime library (Python Docs)](https://docs.python.org/3/library/datetime.html)
- [C# DateTime Class (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?redirectedfrom=MSDN&view=netframework-4.8)