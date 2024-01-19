---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

比较两个日期就是检查两个给定日期之间的差异。作为程序员，我们经常需要执行此操作以满足各种业务需求，如计算年龄、有效期限、工作周等。

## 如何:

```PowerShell
# 创建两个日期的对象
$date1 = Get-Date -Year 2020 -Month 10 -Day 10
$date2 = Get-Date -Year 2021 -Month 05 -Day 20

# 比较两个日期
if ($date1 -eq $date2) {
    Write-Output "两个日期相同"
} elseif ($date1 -gt $date2) {
    Write-Output "date1早于date2"
} else {
    Write-Output "date2晚于date1"
}
```
预期的输出:

```PowerShell
date2晚于date1
```

## 深度剖析

在PowerShell首次发布(2006年)的早期版本中，比较两个日期相对比较复杂并且语法冗长。但是在PowerShell 3.0之后的版本中(2012年发布)，这个过程变得更加简洁而直观。

你还可以使用`.Compare()`方法或者`-lt`、`-gt`和`-eq`等比较运算符作为替代方式来比较日期。例如，你可以用`[datetime]::Compare($date1, $date2)`来比较日期。

在PowerShell对日期进行比较的过程中，实际上比较的是两个日期的Unix时间戳。Unix时间戳是从1970年1月1日开始至今的秒数，这是一个方便且通用的计算日期差异的方式。

## 参见

PowerShell官方文档关于日期对比的相关内容和示例：
- [Get-Date](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.utility/get-date?view=powershell-7.1)
- [About Comparison Operators](https://docs.microsoft.com/zh-cn/powershell/module/microsoft.powershell.core/about/about_comparison_operators?view=powershell-7.2)
- [DateTime Struct](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime?view=net-6.0)