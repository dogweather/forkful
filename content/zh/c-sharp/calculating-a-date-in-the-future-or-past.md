---
title:                "计算未来或过去日期"
html_title:           "C#: 计算未来或过去日期"
simple_title:         "计算未来或过去日期"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

为了帮助你规划你的日程，或者为了解决一个需要预测日期的编程问题，你可能需要计算未来或过去的日期。

## 怎样做

计算未来或过去的日期在C#中非常简单。下面是几个示例和输出：

```C#
// 计算未来一天的日期:
DateTime tomorrow = DateTime.Today.AddDays(1);
Console.WriteLine(tomorrow); // 输出：2021/01/16

// 计算一个月之后的日期:
DateTime nextMonth = DateTime.Today.AddMonths(1);
Console.WriteLine(nextMonth); // 输出：2021/02/15

// 计算未来一年的日期:
DateTime nextYear = DateTime.Today.AddYears(1);
Console.WriteLine(nextYear); // 输出：2022/01/15
```

## 深入了解

在C#中，日期和时间被视为一个对象。DateTime类包含了许多成员方法来帮助我们在日期和时间之间进行计算。其中，AddDays()、AddMonths()和AddYears()方法允许我们在给定的日期上增加指定数目的天数、月份或年份。如果要计算过去的日期，只需要将参数值变为负数即可。

## 参考链接

- [DateTime.AddXXX() 方法](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime.adddays?view=net-5.0)
- [日期和时间处理教程](https://www.w3schools.com/cs/cs_dates.aspx)