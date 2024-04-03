---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:09:11.363775-07:00
description: "\u5728C#\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\u53CA\u4ECE\
  \u7CFB\u7EDF\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u548C\u65F6\u95F4\u7684\u8BE6\
  \u7EC6\u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u9700\u8981\u8BBF\u95EE\u8FD9\
  \u4E9B\u4FE1\u606F\u4EE5\u8FDB\u884C\u65E5\u5FD7\u8BB0\u5F55\u3001\u65F6\u95F4\u6233\
  \u64CD\u4F5C\u6216\u5728\u5E94\u7528\u7A0B\u5E8F\u4E2D\u5B89\u6392\u4EFB\u52A1\uFF0C\
  \u786E\u4FDD\u64CD\u4F5C\u51C6\u65F6\u8FDB\u884C\u5E76\u4E14\u6570\u636E\u5E26\u6709\
  \u7CBE\u786E\u7684\u65F6\u95F4\u6233\u3002"
lastmod: '2024-03-13T22:44:47.779367-06:00'
model: gpt-4-0125-preview
summary: "\u5728C#\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u6D89\u53CA\u4ECE\u7CFB\
  \u7EDF\u4E2D\u83B7\u53D6\u5F53\u524D\u65E5\u671F\u548C\u65F6\u95F4\u7684\u8BE6\u7EC6\
  \u4FE1\u606F\u3002\u7A0B\u5E8F\u5458\u7ECF\u5E38\u9700\u8981\u8BBF\u95EE\u8FD9\u4E9B\
  \u4FE1\u606F\u4EE5\u8FDB\u884C\u65E5\u5FD7\u8BB0\u5F55\u3001\u65F6\u95F4\u6233\u64CD\
  \u4F5C\u6216\u5728\u5E94\u7528\u7A0B\u5E8F\u4E2D\u5B89\u6392\u4EFB\u52A1\uFF0C\u786E\
  \u4FDD\u64CD\u4F5C\u51C6\u65F6\u8FDB\u884C\u5E76\u4E14\u6570\u636E\u5E26\u6709\u7CBE\
  \u786E\u7684\u65F6\u95F4\u6233\u3002."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

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
