---
aliases:
- /zh/c-sharp/converting-a-date-into-a-string/
date: 2024-01-20 17:36:16.335637-07:00
description: "\u5C06\u65E5\u671F\u8F6C\u6362\u6210\u5B57\u7B26\u4E32\u5C31\u662F\u628A\
  \u65E5\u671F\u6570\u636E\u53D8\u6210\u53EF\u8BFB\u6587\u672C\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u4E3A\u4E86\u663E\u793A\u6570\u636E\u3001\u65E5\u5FD7\u8BB0\u5F55\
  \u6216\u8005\u4E0E\u5176\u4ED6\u7CFB\u7EDF\u4EA4\u4E92\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:59.142694
model: gpt-4-1106-preview
summary: "\u5C06\u65E5\u671F\u8F6C\u6362\u6210\u5B57\u7B26\u4E32\u5C31\u662F\u628A\
  \u65E5\u671F\u6570\u636E\u53D8\u6210\u53EF\u8BFB\u6587\u672C\u3002\u7A0B\u5E8F\u5458\
  \u8FD9\u6837\u505A\u4E3A\u4E86\u663E\u793A\u6570\u636E\u3001\u65E5\u5FD7\u8BB0\u5F55\
  \u6216\u8005\u4E0E\u5176\u4ED6\u7CFB\u7EDF\u4EA4\u4E92\u3002"
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
---

{{< edit_this_page >}}

## 什么 & 为什么？
将日期转换成字符串就是把日期数据变成可读文本。程序员这样做为了显示数据、日志记录或者与其他系统交互。

## 如何：
```C#
using System;
using System.Globalization;

public class DateToStringExample
{
    public static void Main()
    {
        DateTime currentDate = DateTime.Now;
        
        // 默认的日期到字符串转换
        string defaultString = currentDate.ToString();
        Console.WriteLine(defaultString);  // 输出如 "2023-03-14 21:13:18"

        // 自定义格式
        string customFormat = currentDate.ToString("yyyy年MM月dd日");
        Console.WriteLine(customFormat);  // 输出如 "2023年03月14日"
        
        // 使用特定文化背景格式
        CultureInfo culture = new CultureInfo("zh-CN");
        string specificCultureFormat = currentDate.ToString(culture);
        Console.WriteLine(specificCultureFormat);  // 输出如 "2023/3/14 21:13:18"
    }
}
```

## 深入探究
早期的程序设计语言没有内建的日期和时间类型。随着时间的推移，需求增加，这些功能被集成到语言和库中。在C#中，.NET框架提供了`DateTime`类型来处理日期和时间。

有很多将日期转换为字符串的方法：
- `ToString()`可以用来转换成默认格式或自定义的格式字符串。
- `String.Format()`可以用来创建格式化的字符串。
- `DateTimeOffset`可用于包含时区的转换。

实施细节包括考虑文化差异以及处理多种日期表示法。比如，在中国，日期通常是年月日格式，而在美国，则是月日年。在C#中，`CultureInfo`类帮助处理这类文化相关的格式化工作。

## 参考
- [.NET DateTime.ToString 方法文档](https://docs.microsoft.com/zh-cn/dotnet/api/system.datetime.tostring)
- [C# String.Format 方法](https://docs.microsoft.com/zh-cn/dotnet/api/system.string.format)
- [C# CultureInfo 类](https://docs.microsoft.com/zh-cn/dotnet/api/system.globalization.cultureinfo)
