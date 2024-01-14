---
title:    "C#: 将日期转换为字符串"
keywords: ["C#"]
---

{{< edit_this_page >}}

为什么：将日期转换为字符串是很常见的编程任务，因为它可以帮助我们将日期以更可读的方式呈现给用户。此外，将日期转换为字符串也可以方便我们在不同的程序中进行数据交互。

如何：下面我们将介绍如何使用C#编程语言来将日期转换为字符串。首先，我们需要创建一个DateTime对象，该对象包含我们想要转换的日期。然后，我们可以使用ToString()方法来将日期转换为字符串，如下所示：

```C#
DateTime date = new DateTime(2021, 11, 11); // 创建DateTime对象
string dateString = date.ToString(); // 将日期转换为字符串
Console.WriteLine(dateString); // 输出: 2021/11/11（具体格式可能会根据电脑系统的设置而有所不同）
```

我们也可以使用ToString()方法的重载版本来指定日期的格式，如下所示：

```C#
DateTime date = new DateTime(2021, 11, 11); // 创建DateTime对象
string dateString = date.ToString("dd-MM-yyyy"); // 将日期转换为字符串，并指定格式为：日-月-年
Console.WriteLine(dateString); // 输出: 11-11-2021
```

深入：除了简单地将日期转换为字符串，我们还可以使用C#中的DateTimeFormat类来更灵活地控制日期的格式。该类提供了各种预定义的格式，也可以自定义格式。下面是一个例子：

```C#
DateTimeFormatInfo format = new DateTimeFormatInfo(); // 创建DateTimeFormatInfo类的实例
format.ShortDatePattern = "yyyy/dd/MMM"; // 自定义日期格式为：年/日/月（月份为三个字母缩写）
DateTime date = new DateTime(2021, 11, 11);
string dateString = date.ToString("d", format); // 对日期使用自定义格式
Console.WriteLine(dateString); // 输出: 2021/11/Nov
```

参考链接：

- [C#官方文档 - DateTime.ToString()方法](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime.tostring?view=net-5.0)
- [C#官方文档 - DateTimeFormat类](https://docs.microsoft.com/zh-cn/dotnet/api/system.globalization.datetimeformatinfo?view=net-5.0)

另请参阅：

- [C#官方文档 - 格式化日期和时间字符串](https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/how-to-convert-a-string-to-a-datetime?view=net-5.0#format-a-datetime-as-a-string)