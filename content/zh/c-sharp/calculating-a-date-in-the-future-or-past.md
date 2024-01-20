---
title:                "计算未来或过去的日期"
html_title:           "C#: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

计算未来或过去的日期是一个常用的编程问题，需要设定一个日期然后增加或减少一段时间。程序员经常需要进行这种计算来预测和追溯事件。

## 如何进行：
```C#
using System;

public class Program
{
    public static void Main()
    {
        DateTime today = DateTime.Today;//获取今天的日期
        DateTime futureDate = today.AddDays(10);//获取10天后的日期
        DateTime pastDate = today.AddDays(-10);//获取10天前的日期
        Console.WriteLine($"今天的日期为: {today}");
        Console.WriteLine($"10天后的日期为: {futureDate}");
        Console.WriteLine($"10天前的日期为: {pastDate}");
    }
}
```
输出：
```txt
今天的日期为: 2022/02/15
10天后的日期为: 2022/02/25
10天前的日期为: 2022/2/5
```

## 深度研究

历史上，我们曾经使用一系列复杂的算法来进行日期计算，但是在现代编程语言如C#中，已经内置了这种功能。对于这种简单的日期计算，您可以直接使用`AddDays`方法。如果您需要进行更复杂的日期计算，如时区和日历的转换，可以使用`DateTimeOffset`和`Calendar`类。

除了C#内置的方式，还有其他的日期计算库如NodaTime。NodaTime提供了更强大和灵活的日期计算功能。

需要注意的是，在执行日期计算时，尽可能避免直接操作日期的组成部分（如年、月、日）。因为这样可能会引发一些边界问题（如闰年和每个月天数的不同）。

## 参见

请参考以下链接以了解更多相关信息：

1. [C# DateTime MSDN文档](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)
2. [C# DateTimeOffset MSDN文档](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=netcore-3.1)
3. [C# Calendar MSDN文档](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.calendar?view=netcore-3.1)
4. [NodaTime Github](https://github.com/nodatime/nodatime)