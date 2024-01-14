---
title:                "C#: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

为什么：日期和字符串是C#中常用的数据类型，因此将日期转换为字符串是一个常见的编程任务。这样可以让我们更方便地处理日期数据，如将其存储到数据库中或者在界面中进行显示。

如何进行日期和字符串的转换呢？在C#中，可以使用DateTime类型的ToString()方法来将日期转换为字符串。具体操作如下所示：

```C#
// 创建一个DateTime类型的对象，表示当前时间
DateTime currentTime = DateTime.Now;
// 将日期转换为“年-月-日”的格式
string dateString = currentTime.ToString("yyyy-MM-dd");
// 输出转换后的字符串
Console.WriteLine(dateString);
```

运行该段代码，将会在控制台输出当前日期的“年-月-日”格式字符串，例如：“2021-12-25”。

深入了解日期和字符串的转换，我们需要了解DateTime类型中的ToString()方法的更多用法。该方法可以接受不同的格式字符串作为参数，来控制转换后的日期格式。例如：

- "MMMM"：输出月份的全名，如"January"；
- "yy"：输出两位数的年份，如"21"。

更多可用的格式字符串可以在微软的官方文档中查看。

除了通过ToString()方法来进行日期和字符串的转换外，C#中也有其他方法可以实现相同的功能，如DateTime.Parse()和string.Format()。有兴趣的读者可以自行探索它们的使用方法。

## See Also

- [DateTime.ToString() 方法 (System) - Microsoft Docs](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime.tostring?view=net-5.0)
- [DateTime.Parse() 方法 (System) - Microsoft Docs](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime.parse?view=net-5.0)
- [string.Format() 方法 (System) - Microsoft Docs](https://docs.microsoft.com/zh-cn/dotnet/api/system.string.format?view=net-5.0)