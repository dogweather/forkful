---
title:                "C#: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么要获取当前日期

在编写任何计算机程序时，经常需要获取当前日期。这可以帮助我们跟踪时间信息，例如记录一个事件发生的日期或计算某个任务的截止日期。获取当前日期也可以用于编写可自动更新的日志文件或报表。无论是什么原因，获取当前日期都是一项非常重要的任务。

## 如何获取当前日期

在C#编程语言中，获取当前日期非常简单。我们可以使用DateTime类中的Now属性来获取当前日期和时间。以下是一个简单的示例代码，展示如何使用Now属性来获取当前日期和时间，并将其打印出来：

```C#
DateTime now = DateTime.Now;
Console.WriteLine(now);
```

该代码的输出将会是当前日期和时间，类似于"7/20/2021 10:05:12 AM"。我们也可以使用ToString方法来自定义输出日期格式，比如只输出日期或只输出时间。以下是一个示例代码，展示如何使用ToString方法来自定义输出日期格式：

```C#
DateTime now = DateTime.Now;
Console.WriteLine(now.ToString("yyyy/MM/dd"));
Console.WriteLine(now.ToString("hh:mm:ss tt"));
```

该代码的输出将会是当前日期，类似于"2021/07/20"，以及当前时间，类似于"10:05:12 AM"。

## 深入了解获取当前日期

在C#编程语言中，DateTime类中的Now属性实际上是调用了系统的GetSystemTime方法来获取当前日期和时间。这个方法返回的是一个DateTime数据类型，其中包含了当前日期和时间的详细信息，例如年份、月份、日期、小时、分钟和秒数。因此，我们可以选择从返回的DateTime数据类型中提取我们需要的特定信息。

此外，DateTime类中还有许多有用的方法可以帮助我们处理日期和时间，例如Add方法来计算某个日期后的日期，Subtract方法来计算两个日期之间的时间间隔。熟悉这些方法可以让我们更加灵活地处理日期和时间。

## 参考链接

- Microsoft文档：[DateTime.Now 属性](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime.now?view=net-5.0)
- C#教程：[DateTime类和其方法](https://www.w3schools.com/cs/cs_datetime.asp)
- 代码示例：[如何用C#获取当前日期和时间？](https://www.codegrepper.com/code-examples/csharp/c%23+current+date+and+time)