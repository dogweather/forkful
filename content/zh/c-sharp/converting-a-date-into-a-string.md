---
title:                "将日期转换为字符串"
html_title:           "C#: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

日期的处理在编程中是一项基本的任务，但是在实际开发中，经常会遇到需要将日期转换为字符串的场景。比如在日志记录、显示日期等功能中，都需要将日期转换为特定的格式的字符串。因此，了解如何将日期转换为字符串是非常重要的，它可以帮助开发人员更高效地处理日期相关的任务。

## 如何实现

在C＃中，将日期转换为字符串的最简单的方法就是使用DateTime对象的ToString（）方法。以下是一个例子，将一个日期转换为特定格式的字符串，可以看到输出结果为"2020/7/11"：

```C#
DateTime date = new DateTime(2020, 07, 11);

string dateString = date.ToString("yyyy/M/d");

Console.WriteLine(dateString); // 输出结果：2020/7/11
```

除了常用的"yyyy/M/d"格式外，C＃还提供了许多其他格式，可以根据需要进行选择。例如，可以使用“dd/MM/yyyy”来将日期转换为格式为“dd/mm/yyyy”的字符串。

此外，还可以使用DateTime类的其他方法来实现更复杂的日期转换操作。例如，通过使用DateTime.ParseExact()方法，可以将特定格式的字符串转换为日期类型。

## 深入了解

在C＃中，日期转换的底层机制涉及到一个名为"DateTimeFormatInfo"的类。它包含了各种日期格式的定义，比如星期几、月份等，同时也可以通过修改该类的属性来自定义日期的格式。

除了ToString（）方法外，C＃还提供了其他一些有用的方法，如DateTime.Parse（）和DateTime.TryParse（），它们可以将字符串转换为DateTime对象，非常适合在用户输入日期的场景中使用。

## 参考链接

- [C# DateTime转换为字符串的详细教程](https://www.cnblogs.com/yiylunhui/p/12125471.html)
- [Microsoft官方文档：DateTime.ToString()方法](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime.tostring?view=netcore-3.1)