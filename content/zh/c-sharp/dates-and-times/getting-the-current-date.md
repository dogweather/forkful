---
title:                "获取当前日期"
aliases:
- /zh/c-sharp/getting-the-current-date.md
date:                  2024-02-03T19:09:11.363775-07:00
model:                 gpt-4-0125-preview
simple_title:         "获取当前日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
在C#中获取当前日期涉及从系统中获取当前日期和时间的详细信息。程序员经常需要访问这些信息以进行日志记录、时间戳操作或在应用程序中安排任务，确保操作准时进行并且数据带有精确的时间戳。

## 如何操作：
C#通过`DateTime`类提供了一种直截了当的方式来获取当前日期，该类是.NET框架的System命名空间的一部分。下面的示例演示了如何获取当前日期，以及（可选的）时间。

```csharp
using System;

class Program
{
    static void Main()
    {
        // 仅获取当前日期
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // 输出：MM/dd/yyyy

        // 获取当前日期和时间
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // 输出：MM/dd/yyyy HH:mm:ss

        // 获取当前的UTC日期和时间
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // 输出：MM/dd/yyyy HH:mm:ss
    }
}
```

就第三方库而言，NodaTime提供了一个健壮的选择，用于日期和时间的操作，包括以不同日历和时区获取当前日期。

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // 使用NodaTime以ISO日历获取当前日期
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // 输出：yyyy-MM-dd

        // 对于特定时区的日期
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // 输出：yyyy-MM-dd
    }
}
```

这展示了使用内置的`DateTime`类的基本用法，以及NodaTime提供的增强功能，特别适用于需要处理不同时区或日历系统的应用程序。
