---
title:    "C#: 获取当前日期"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

为什么要获取当前日期？日常生活中，我们经常需要使用当前日期来记录时间或进行一些特定的计算。在编写程序时，获取当前日期也是非常常见的需求，比如在日志记录、数据存储、任务调度等场景中。因此，了解如何获取当前日期是一项基本的编程技能。

## 如何做

在C#中，可以使用DateTime类来获取当前日期。首先，需要在程序中引入System命名空间，然后创建一个DateTime对象，即可获取当前日期。下面的代码示例演示了如何使用DateTime类来获取当前日期，并通过控制台打印输出：

```
using System;

DateTime currentDate = DateTime.Now;
Console.WriteLine("当前日期为：" + currentDate);
```

运行上述代码，会在控制台输出类似以下结果：

```
当前日期为：2021/9/27 上午 9:20:50
```

除了获取当前日期外，我们也可以通过DateTime类的其他方法来获取年、月、日、时、分、秒等具体的时间信息。比如，下面的代码演示了如何获取当前日期的年份：

```
using System;

DateTime currentDate = DateTime.Now;
int currentYear = currentDate.Year;
Console.WriteLine("当前年份为：" + currentYear);
```

运行以上代码，会在控制台输出当前的年份，比如：

```
当前年份为：2021
```

## 深入了解

DateTime类是C#中用于处理日期和时间的核心类，除了可以获取当前日期外，它还提供了一系列方法来对日期进行操作和计算。例如，可以使用AddDays()方法来对日期进行加减天数的计算，或者使用ToString()方法来自定义日期的格式化输出。

此外，C#中还有其他相关的日期时间类，比如DateTimeOffset和TimeSpan，它们分别用于表示带有时区信息的日期时间和时间间隔。在实际开发中，可以根据具体的需求来选择合适的日期时间类来处理日期相关的业务逻辑。

## 参考资料

- [DateTime Class (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [How to: Get the Current Date and Time (C# Programming Guide) (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/inside-a-program/how-to-get-the-current-date-and-time)