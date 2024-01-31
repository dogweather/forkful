---
title:                "从字符串解析日期"
date:                  2024-01-20T15:35:19.033574-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"

category:             "C#"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么 & 为什么？
将字符串解析为日期是提取字符串中包含的日期信息并将其转换为程序可以理解和操作的日期对象的过程。程序员这么做以便可以对日期进行计算、比较和格式化。

## How to: 怎么做？
```C#
using System;
using System.Globalization;

public class DateParsingExample
{
    static void Main()
    {
        string dateString = "2023-04-01";
        DateTime parsedDate;
        
        if(DateTime.TryParseExact(dateString, "yyyy-MM-dd", 
                                  CultureInfo.InvariantCulture, 
                                  DateTimeStyles.None, out parsedDate))
        {
            Console.WriteLine($"成功解析: {parsedDate}");
        }
        else
        {
            Console.WriteLine("字符串解析失败。");
        }
    }
}
```
输出:
```
成功解析: 2023/4/1 0:00:00
```

## Deep Dive 深入探讨
解析字符串为日期有很长的历史，自编程出现起就非常重要。过去，解析非标准格式的日期字符串需要大量手写代码。现在，.NET提供了丰富的库（如`DateTime`和`DateTimeOffset`）来简化这个过程。

除了`DateTime.TryParseExact`方法，还有`DateTime.Parse`和`DateTime.TryParse`方法，在处理不同的日期格式或者需要更灵活的解析时很有用。但是，使用`DateTime.TryParseExact`可以避免由于不明确的日期格式导致的潜在错误。

在底层实现上，日期字符串解析依赖于文化特定信息，使用`CultureInfo`可以确保按照特定的文化约定解析日期。在全球化的应用中，正确处理不同文化的日期格式至关重要。

## See Also 相关资源
- [DateTime Struct](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0) - 官方文档关于DateTime 结构.
- [.NET Globalization and Localization](https://docs.microsoft.com/en-us/dotnet/standard/globalization-localization/) - 关于.NET全球化和本地化的官方指南.
- [Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings) - 自定义日期和时间格式字符串的文档.
