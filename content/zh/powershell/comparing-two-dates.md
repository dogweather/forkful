---
title:                "比较两个日期"
html_title:           "PowerShell: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么是日期比较？为什么程序员要这么做？

日期比较是指比较两个日期之间的关系，如相等、早于或晚于。程序员经常需要比较日期，以便在他们的代码中根据日期的顺序做出不同的操作。比如，他们可能需要找出所有在某个特定日期之后发生的事件，或者按照日期顺序输出一系列数据。

## 如何进行日期比较？

在PowerShell中，比较两个日期可以通过几种方式实现。我们可以使用“-lt”（小于），“-eq”（等于）和“-gt”（大于）这些运算符来判断日期的关系。让我们来看一个例子：

```PowerShell
$date1 = Get-Date -Date "2021/05/10"
$date2 = Get-Date -Date "2021/05/15"

if($date1 -lt $date2){
    Write-Host "$date1 早于 $date2"
}
elseif($date1 -gt $date2){
    Write-Host "$date1 晚于 $date2"
}
else{
    Write-Host "$date1 和 $date2 相等"
}
```

输出结果将是：`2021年5月10日早于2021年5月15日`。

## 深入了解日期比较

历史背景：在计算机科学的早期，日期比较是一个很困难的问题。因为日期在不同的国家和文化中有不同的表示方法，导致在比较时经常出现错误。不过，计算机科学家们开发了通用的日期比较算法，让我们能够方便地在代码中比较日期。

其它选择：除了使用运算符进行日期比较外，我们也可以使用一些PowerShell函数，如`Compare-Object`和`New-TimeSpan`，来检测日期之间的差异和间隔。

实现细节：在比较日期时，PowerShell会自动将日期格式转换为一个整数，称为`Ticks`。这样比较起来更容易，因为我们只需要比较两个整数的大小。

## 参考资料

- ["Working with Dates and Times in PowerShell"](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/?view=powershell-7.1#working-with-dates-and-times) by Microsoft
- ["PowerShell Date and Time Format in PowerShell"](https://www.sqlshack.com/date-and-time-format-in-powershell/) by Rajendra Gupta
- ["DateTime Ticks in PowerShell"](https://mcpmag.com/articles/2017/11/14/datetime-ticks.aspx) by Boe Prox