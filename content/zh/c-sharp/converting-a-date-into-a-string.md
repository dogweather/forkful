---
title:    "C#: 将日期转换为字符串"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

为什么：只需要1-2句话来解释为什么某人会进行日期转换成字符串的操作。

## 为什么

在编程中，日期和时间在很多情况下都是必不可少的。有时候我们需要将日期和时间作为字符串进行处理，比如在输出日志或者保存数据时。因此，将日期转换成字符串是一项常见的操作。

## 如何进行日期转换

要将日期转换成字符串，我们可以使用C#中的ToString()方法。这个方法可以将日期按照指定的格式转换成字符串。让我们来看一个例子：
```C#
DateTime now = DateTime.Now;
// 将日期转换成字符串
string date = now.ToString("MM/dd/yyyy");
// 输出结果为 09/21/2021
Console.WriteLine(date);
```

我们可以在ToString()方法中传入不同的格式来得到不同的字符串，比如"dd/MM/yyyy"、"M/d/yyyy"等。具体的格式可以根据自己的需求来定制。

有时候，我们也需要将字符串转换成日期。此时，我们可以使用C#中的Parse()方法。这个方法可以将字符串转换成日期类型。让我们来看一个例子：
```C#
string strDate = "09/21/2021";
// 将字符串转换为日期
DateTime date = DateTime.Parse(strDate);
// 输出结果为 09/21/2021 12:00:00 AM
Console.WriteLine(date);
```

一般来说，C#中的日期格式和语言的日期格式是不同的。因此，在进行日期转换时需要注意格式的问题。

## 深入了解日期转换

在C#中，日期和时间的处理是基于DateTime结构体的。这个结构体提供了一系列方法来处理日期和时间，包括ToString()和Parse()方法。在实际开发中，我们还可以使用其他的方法来处理日期和时间，比如Add()、Subtract()等等。

此外，C#中还有一个重要的类叫做DateTimeFormatter，它提供了更加灵活的日期格式转换功能。我们可以根据自己的需求来定制格式，比如国际化的日期格式、24小时制和12小时制的时间等等。

## 参考链接

- [DateTime.ToString()方法文档](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tostring?view=net-5.0)
- [DateTime.Parse()方法文档](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=net-5.0)
- [DateTime 结构体文档](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [DateTimeFormatter 类文档](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.datetimeformatter?view=net-5.0)

## 参见

如果你想了解更多关于C#中日期和时间的处理，可以参考下面的链接：
- [日期和时间教程](https://docs.microsoft.com/en-us/dotnet/standard/base-types/date-time)
- [日期和时间格式化指南](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)