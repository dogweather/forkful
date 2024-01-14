---
title:                "C#: 将日期转换为字符串"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

一般性的C#程序设计博文，读者对象为中文读者，分为"I. 为什么", "II. 如何", "III. 深入探讨"三个部分。


# 为什么：
日期和时间是编程中常见的数据类型，经常需要在不同的场景中将其转换成字符串格式。例如，在日志记录中，我们通常需要将当前时间以字符串形式输出。通过将日期转换为字符串，我们可以灵活地控制其格式和展示方式。

# 如何：
要将日期转换为字符串，首先需要将其存储在DateTime对象中。然后使用ToString()方法，指定所需的字符串格式。

```C#
// 示例代码
DateTime currentDate = DateTime.Now; // 获取当前日期和时间
string dateAsString = currentDate.ToString("yyyy/MM/dd"); // 设置字符串格式为年/月/日
Console.WriteLine(dateAsString); // 输出: 2021/07/05
```

# 深入探讨：
除了可以指定日期格式外，我们还可以使用标准日期和时间格式字符串来实现更灵活的日期转换。例如，我们可以使用"F"格式字符串来输出完整的日期和时间信息，或者使用"g"格式字符串来输出简短的日期和时间信息。

此外，C#还提供了一系列用于格式化日期的特定格式的符号。通过将这些符号与自定义字符串一起使用，我们可以构建出自己想要的日期格式。

## See Also:
- [C# DateTime 类](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime)
- [标准日期和时间格式字符串](https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/standard-date-and-time-format-strings)
- [自定义日期和时间格式字符串](https://docs.microsoft.com/zh-cn/dotnet/standard/base-types/custom-date-and-time-format-strings)