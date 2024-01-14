---
title:    "C#: 获取当前日期"
keywords: ["C#"]
---

{{< edit_this_page >}}

# 为什么要获取当前日期

在编程的世界里，获取当前日期是一个常见的需求。无论是用于计算日期的差异，或者简单地显示当前日期，获取当前日期都是必须要做的事情。在C#编程语言中，我们有许多方法来获取当前日期和时间。让我们看看如何实现它。

## 如何实现

有几种方法可以获取当前日期。我们可以使用 `DateTime` 对象来获取当前日期和时间。让我们看看下面的代码示例：

```C#
// 获取当前日期和时间
DateTime currentDateTime = DateTime.Now;

// 获取当前日期
DateTime currentDate = DateTime.Today;

// 获取当前时间
DateTime currentTime = DateTime.Now.TimeOfDay;

// 显示日期和时间的不同部分
Console.WriteLine($"当前日期和时间: {currentDateTime}");
Console.WriteLine($"当前日期: {currentDate}");
Console.WriteLine($"当前时间: {currentTime}");
```

这段代码会输出以下结果：

```
当前日期和时间: 8/21/2021 9:05:23 PM
当前日期: 8/21/2021 12:00:00 AM
当前时间: 9:05:23 PM
```

除了使用 `DateTime` 对象，我们也可以使用 `DateTime.Now` 方法来获取当前日期和时间。这个方法会返回一个 `DateTime` 对象，包含当前日期和时间的信息。

## 深入了解

为了更深入地了解如何获取当前日期，我们需要知道日期是如何在计算机中表示的。在C#中，日期和时间都是被存储为 `DateTime` 对象。这个对象包含了日期和时间的信息，可以通过它的属性来获取。

日期在计算机中是以一个数字来表示的，这个数字表示从特定日期（通常是1970年1月1日）到现在经过的秒数。这个数字称为epoch时间戳。所以实际上，获取当前日期和时间就是通过计算当前的epoch时间戳来实现的。

## 参考链接

- [C# 中的日期和时间](https://docs.microsoft.com/zh-cn/dotnet/csharp/programming-guide/classes-and-structs/dates-and-times)
- [DateTime类](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime?view=net-5.0)