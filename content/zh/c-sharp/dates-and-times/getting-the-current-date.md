---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:11.363775-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A C#\u901A\u8FC7`DateTime`\u7C7B\u63D0\u4F9B\
  \u4E86\u4E00\u79CD\u76F4\u622A\u4E86\u5F53\u7684\u65B9\u5F0F\u6765\u83B7\u53D6\u5F53\
  \u524D\u65E5\u671F\uFF0C\u8BE5\u7C7B\u662F.NET\u6846\u67B6\u7684System\u547D\u540D\
  \u7A7A\u95F4\u7684\u4E00\u90E8\u5206\u3002\u4E0B\u9762\u7684\u793A\u4F8B\u6F14\u793A\
  \u4E86\u5982\u4F55\u83B7\u53D6\u5F53\u524D\u65E5\u671F\uFF0C\u4EE5\u53CA\uFF08\u53EF\
  \u9009\u7684\uFF09\u65F6\u95F4\u3002"
lastmod: '2024-03-13T22:44:47.779367-06:00'
model: gpt-4-0125-preview
summary: "C#\u901A\u8FC7`DateTime`\u7C7B\u63D0\u4F9B\u4E86\u4E00\u79CD\u76F4\u622A\
  \u4E86\u5F53\u7684\u65B9\u5F0F\u6765\u83B7\u53D6\u5F53\u524D\u65E5\u671F\uFF0C\u8BE5\
  \u7C7B\u662F.NET\u6846\u67B6\u7684System\u547D\u540D\u7A7A\u95F4\u7684\u4E00\u90E8\
  \u5206\u3002\u4E0B\u9762\u7684\u793A\u4F8B\u6F14\u793A\u4E86\u5982\u4F55\u83B7\u53D6\
  \u5F53\u524D\u65E5\u671F\uFF0C\u4EE5\u53CA\uFF08\u53EF\u9009\u7684\uFF09\u65F6\u95F4\
  ."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
