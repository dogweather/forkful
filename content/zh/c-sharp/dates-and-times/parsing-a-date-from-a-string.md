---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:50.813839-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A **\u57FA\u672C\u89E3\u6790\uFF1A** `DateTime.Parse`\
  \ \u548C `DateTime.TryParse` \u65B9\u6CD5\u662F\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\
  \u4E3A `DateTime` \u7684\u9996\u9009\u9009\u9879\u3002\u8FD9\u662F\u4E00\u4E2A\u5FEB\
  \u901F\u793A\u4F8B\uFF1A."
lastmod: '2024-03-13T22:44:47.778060-06:00'
model: gpt-4-0125-preview
summary: "**\u57FA\u672C\u89E3\u6790\uFF1A**\n\n`DateTime.Parse` \u548C `DateTime.TryParse`\
  \ \u65B9\u6CD5\u662F\u5C06\u5B57\u7B26\u4E32\u8F6C\u6362\u4E3A `DateTime` \u7684\
  \u9996\u9009\u9009\u9879\u3002\u8FD9\u662F\u4E00\u4E2A\u5FEB\u901F\u793A\u4F8B\uFF1A\
  ."
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
weight: 30
---

## 如何操作：
**基本解析：**

`DateTime.Parse` 和 `DateTime.TryParse` 方法是将字符串转换为 `DateTime` 的首选选项。这是一个快速示例：

```csharp
string dateString = "2023-04-12";
DateTime parsedDate;

if (DateTime.TryParse(dateString, out parsedDate))
{
    Console.WriteLine($"成功解析: {parsedDate}");
}
else
{
    Console.WriteLine("解析失败。");
}
// 输出: 成功解析: 2023/4/12 上午 12:00:00
```

**指定文化：**

有时，你需要解析一个特定文化格式的日期字符串。你可以使用 `CultureInfo` 类来实现这一点：

```csharp
using System.Globalization;

string dateString = "12 avril 2023";
var cultureInfo = new CultureInfo("fr-FR");
DateTime parsedDate = DateTime.Parse(dateString, cultureInfo);

Console.WriteLine(parsedDate);
// 输出: 2023/4/12 上午 12:00:00
```

**使用具体格式进行精确解析：**

在日期以可能不是标准的特定格式存在的情况下，`DateTime.ParseExact` 这一方法十分有用：

```csharp
string dateString = "Wednesday, 12 April 2023";
string format = "dddd, d MMMM yyyy";
DateTime parsedDate = DateTime.ParseExact(dateString, format, CultureInfo.InvariantCulture);

Console.WriteLine(parsedDate);
// 输出: 2023/4/12 上午 12:00:00
```

**使用 NodaTime：**

为了更强大的日期和时间解析，考虑使用流行的第三方库 NodaTime。它提供更广泛的日期/时间处理能力：

```csharp
using NodaTime;
using NodaTime.Text;

var pattern = LocalDatePattern.CreateWithInvariantCulture("yyyy-MM-dd");
var parseResult = pattern.Parse("2023-04-12");

if (parseResult.Success)
{
    LocalDate localDate = parseResult.Value;
    Console.WriteLine(localDate); // 2023-04-12
}
else
{
    Console.WriteLine("解析失败。");
}
```

NodaTime 为时区、时段和持续时间概念以及许多不同的日历系统提供了广泛的支持，使其成为 .NET 应用程序中处理复杂日期和时间操作的一个强大选择。
