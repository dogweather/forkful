---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
获取当前日期是编程操作中的一种，使你能获取和使用计算机系统中的实时日期。这对于记录事件发生的时间点，满足特定的系统要求，如日志记录，以及满足特定的功能需求，如日期和时间戳，都是有用之处。

## 如何实现：
在C#中，获取当前日期是一项简单的操作。你只需要使用DateTime的Now属性：

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

这将输出：

```C#
2022-01-01 12:00:00
```

注意，这里的输出将会是你运行代码时的实时日期和时间。

## 深度解读：

**历史背景**：在早期的编程语言中，获取当前日期并不是轻而易举的事，需要调用操作系统的特定函数。但现在，多数高级语言，像C#，都有内建的日期和时间类，使得操作变得简单易行。

**可选方法**：除了Now属性，你还可以使用Today属性获得当前日期，该属性将只提供日期，不包含时间：

```C#
DateTime currentDate = DateTime.Today;
Console.WriteLine(currentDate);
```

这将输出：

```C#
2022-01-01 00:00:00
```
你看，时间部分都是零。

**实施细节**：在幕后，.NET运行时基于你的操作系统来获取当前日期和时间。在Windows中，它通过调用Win32 API的GetSystemTime函数来获取。

## 另请参见：
* [MSDN – DateTime.Now Property](https://docs.microsoft.com/en-US/dotnet/api/system.datetime.now?view=net-5.0)
* [MSDN – DateTime.Today Property](https://docs.microsoft.com/en-US/dotnet/api/system.datetime.today?view=net-5.0)
* [StackOverflow Post – DateTime.Now vs DateTime.Today](https://stackoverflow.com/questions/1410127/datetime-now-vs-datetime-today)