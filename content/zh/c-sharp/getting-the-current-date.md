---
title:                "获取当前日期"
html_title:           "C#: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

为了开发程序，我们经常需要获取当前日期。不仅可以使程序更加动态和有用，也可以用来跟踪时间敏感的任务或事件。

## 如何获取当前日期

```C#
//使用DateTime.Now属性来获取当前日期和时间的实例
DateTime currentDateTime = DateTime.Now;

//获取当前日期
DateTime currentDate = currentDateTime.Date;

//获取当前时间
DateTime currentTime = currentDateTime.TimeOfDay;

//输出当前日期和时间
Console.WriteLine("当前日期：{0}", currentDate);
Console.WriteLine("当前时间：{0}", currentTime);
```

**输出：**

当前日期：10/25/2021

当前时间：10:25:00 AM


## 深入了解

除了使用DateTime.Now属性获取当前日期和时间之外，还有其他方法可以获取当前日期。比如，使用DateTime.Today属性来获取当天的日期，不包含当前时间。

```C#
//使用DateTime.Today属性来获取当天的日期
DateTime today = DateTime.Today;
Console.WriteLine("今天的日期：{0}", today);

//输出：今天的日期：10/25/2021
```

此外，还可以使用DateTime.UtcNow属性获取当前的UTC日期和时间。UTC是协调世界时，是一种标准的世界统一时间，与时区无关。

```C#
//使用DateTime.UtcNow属性来获取当前的UTC日期和时间
DateTime utcDateTime = DateTime.UtcNow;
Console.WriteLine("当前的UTC日期和时间：{0}", utcDateTime);

//输出：当前的UTC日期和时间：10/25/2021 2:30:00 AM
```

## 参考链接

- [Microsoft Docs - Date and Time](https://docs.microsoft.com/en-us/dotnet/standard/base-types/date-and-time)
- [C# DateTime Class](https://www.tutorialspoint.com/csharp/csharp_date_time.htm)
- [C# Tutorials - Working with Dates and Time](https://www.c-sharpcorner.com/UploadFile/shivprasadk/csharpdatetimemast06092005021848AM/csharpdatetimemast.aspx)

## 参见

- [C# 字符串格式化：让你的程序多样化](https://example.com/article/using-csharp-string-formatting)
- [掌握C#的条件语句：if、switch和ternary操作符](https://example.com/article/csharp-conditional-statements)