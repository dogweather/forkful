---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:50.813839-07:00
description: "\u5728 C# \u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\
  \u6D89\u53CA\u5C06\u65E5\u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u8FBE\u5F62\
  \u5F0F\u8F6C\u6362\u6210\u4E00\u4E2A `DateTime` \u5BF9\u8C61\u3002\u8FD9\u5BF9\u4E8E\
  \u9700\u8981\u4EE5\u4E0D\u540C\u683C\u5F0F\u64CD\u4F5C\u3001\u5B58\u50A8\u6216\u663E\
  \u793A\u65E5\u671F\u548C\u65F6\u95F4\u7684\u5E94\u7528\u7A0B\u5E8F\u6765\u8BF4\u81F3\
  \u5173\u91CD\u8981\uFF0C\u6BD4\u5982\u65E5\u7A0B\u5B89\u6392\u5E94\u7528\u3001\u65E5\
  \u5FD7\u5904\u7406\u5668\u6216\u4EFB\u4F55\u5904\u7406\u7528\u6237\u6216\u5916\u90E8\
  \u6765\u6E90\u65E5\u671F\u8F93\u5165\u7684\u7CFB\u7EDF\u3002"
lastmod: '2024-03-11T00:14:21.562669-06:00'
model: gpt-4-0125-preview
summary: "\u5728 C# \u4E2D\u89E3\u6790\u5B57\u7B26\u4E32\u4E2D\u7684\u65E5\u671F\u6D89\
  \u53CA\u5C06\u65E5\u671F\u548C\u65F6\u95F4\u7684\u6587\u672C\u8868\u8FBE\u5F62\u5F0F\
  \u8F6C\u6362\u6210\u4E00\u4E2A `DateTime` \u5BF9\u8C61\u3002\u8FD9\u5BF9\u4E8E\u9700\
  \u8981\u4EE5\u4E0D\u540C\u683C\u5F0F\u64CD\u4F5C\u3001\u5B58\u50A8\u6216\u663E\u793A\
  \u65E5\u671F\u548C\u65F6\u95F4\u7684\u5E94\u7528\u7A0B\u5E8F\u6765\u8BF4\u81F3\u5173\
  \u91CD\u8981\uFF0C\u6BD4\u5982\u65E5\u7A0B\u5B89\u6392\u5E94\u7528\u3001\u65E5\u5FD7\
  \u5904\u7406\u5668\u6216\u4EFB\u4F55\u5904\u7406\u7528\u6237\u6216\u5916\u90E8\u6765\
  \u6E90\u65E5\u671F\u8F93\u5165\u7684\u7CFB\u7EDF\u3002"
title: "\u4ECE\u5B57\u7B26\u4E32\u89E3\u6790\u65E5\u671F"
---

{{< edit_this_page >}}

## 什么 & 为什么？
在 C# 中解析字符串中的日期涉及将日期和时间的文本表达形式转换成一个 `DateTime` 对象。这对于需要以不同格式操作、存储或显示日期和时间的应用程序来说至关重要，比如日程安排应用、日志处理器或任何处理用户或外部来源日期输入的系统。

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
