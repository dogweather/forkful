---
date: 2024-01-20 17:28:42.626073-07:00
description: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u662F\u6307\
  \u627E\u51FA\u4E00\u4E2A\u76F8\u5BF9\u4E8E\u5F53\u524D\u6216\u7279\u5B9A\u65E5\u671F\
  \u7684\u5C06\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u70B9\u3002\u7A0B\u5E8F\u5458\
  \u505A\u8FD9\u4E2A\u64CD\u4F5C\u4EE5\u5904\u7406\u9884\u8BA2\u3001\u8BA1\u5212\u3001\
  \u8D85\u65F6\u7B49\u573A\u666F\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:45.350891-07:00'
model: gpt-4-1106-preview
summary: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u662F\u6307\
  \u627E\u51FA\u4E00\u4E2A\u76F8\u5BF9\u4E8E\u5F53\u524D\u6216\u7279\u5B9A\u65E5\u671F\
  \u7684\u5C06\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F\u70B9\u3002\u7A0B\u5E8F\u5458\
  \u505A\u8FD9\u4E2A\u64CD\u4F5C\u4EE5\u5904\u7406\u9884\u8BA2\u3001\u8BA1\u5212\u3001\
  \u8D85\u65F6\u7B49\u573A\u666F\u3002"
title: "\u8BA1\u7B97\u672A\u6765\u6216\u8FC7\u53BB\u7684\u65E5\u671F"
---

{{< edit_this_page >}}

## What & Why? (是什么以及为什么？)
计算未来或过去的日期是指找出一个相对于当前或特定日期的将来或过去的日期点。程序员做这个操作以处理预订、计划、超时等场景。

## How to: (怎么做：)
在C#中，DateTime和TimeSpan是处理日期和时间的基本工具。以下示例显示了如何使用这些工具来计算未来和过去的日期。

```C#
using System;

public class DateCalculator
{
    static void Main()
    {
        DateTime today = DateTime.Now;
        TimeSpan tenDays = TimeSpan.FromDays(10);
        DateTime tenDaysAhead = today.AddDays(10);
        DateTime tenDaysAgo = today.AddDays(-10);

        Console.WriteLine("Today is: " + today.ToShortDateString());
        Console.WriteLine("10 days from now will be: " + tenDaysAhead.ToShortDateString());
        Console.WriteLine("10 days ago was: " + tenDaysAgo.ToShortDateString());
    }
}
```

输出示例：
```
Today is: 03/15/2023
10 days from now will be: 03/25/2023
10 days ago was: 03/05/2023
```

## Deep Dive (深入探讨):
C#的日期和时间API源自.NET框架的早期版本，主要围绕System.DateTime和System.TimeSpan类。这些类提供方法执行日期加减、比较等操作。除了DateTime和TimeSpan，还有DateTimeOffset，推荐用来表示带有时区的时间点。

替代选项包括NodaTime库，由Jon Skeet设计，提供更精细的日期管理。如需涉及复杂时间运算，可能需要此库。

在实现时，注意时区（使用DateTimeOffset）和闰秒。计算未来或过去日期时要处理好这些细节问题。

## See Also (另请参阅):
- Microsoft Docs on DateTime: https://docs.microsoft.com/en-us/dotnet/api/system.datetime
- Microsoft Docs on TimeSpan: https://docs.microsoft.com/en-us/dotnet/api/system.timespan
- NodaTime Documentation: https://nodatime.org/3.0.x/userguide
- Stack Overflow discussion on DateTime vs. DateTimeOffset: https://stackoverflow.com/questions/4331189/datetime-vs-datetimeoffset
