---
title:                "从字符串中解析日期"
date:                  2024-02-01T21:57:33.387067-07:00
model:                 gpt-4-0125-preview
simple_title:         "从字符串中解析日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/vba/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何为及为何？

在 Visual Basic for Applications (VBA) 中解析字符串中的日期是指将代表日期的文本转换为日期数据类型。程序员这样做是为了在他们的应用程序中更有效地操作日期，例如进行比较、计算或格式化。

## 如何做：

VBA 提供了一种直接的方式来使用 `CDate` 函数或 `DateValue` 函数将字符串解析成日期。然而，关键在于字符串必须是可识别的日期格式。

这里有一个使用 `CDate` 的基本例子：

```basic
Sub ParseDateUsingCDate()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "2023-04-01"
    parsedDate = CDate(dateString)
    
    Debug.Print "Parsed Date: "; parsedDate
End Sub
```

如果你运行这段代码，在立即窗口（通过 VBA 编辑器中的 `Ctrl+G` 访问）中的输出将会是：

```
Parsed Date: 4/1/2023 
```

另外，你可以使用 `DateValue` 函数，这个函数更具体地针对日期（忽略时间部分）：

```basic
Sub ParseDateUsingDateValue()
    Dim dateString As String
    Dim parsedDate As Date
    
    dateString = "April 1, 2023"
    parsedDate = DateValue(dateString)
    
    Debug.Print "Parsed Date using DateValue: "; parsedDate
End Sub
```

这个例子的输出同样会显示在立即窗口中：

```
Parsed Date using DateValue: 4/1/2023
```

请记住，解析的成功取决于字符串的日期格式是否与系统或应用程序设置匹配。

## 深入探究

在内部，当 VBA 将字符串解析成日期时，它使用 Windows 操作系统的区域设置来解释日期格式。理解这一点至关重要，因为在一个系统上完美解析的日期字符串可能会在另一个使用不同日期/时间设置的系统上引起错误。

从历史上看，处理日期一直是应用程序中的一个常见错误源，特别是那些在国际上使用的应用程序。VBA 依赖于区域设置的原因，是为什么有些人可能会考虑采用像 ISO 8601 格式（例如，“YYYY-MM-DD”）这样的备选方案，以跨不同系统实现日期表示和解析的无歧义性。不幸的是，VBA 原生不支持 ISO 8601 格式，严格遵循需要手动解析。

对于超出 `CDate` 或 `DateValue` 能处理的复杂日期解析，或为了确保无论系统的区域设置如何都能保持一致的解析，程序员可能会采用自定义解析函数。这些可能涉及将日期字符串分割成组件（年、月、日）并使用 `DateSerial` 函数构造一个日期。其他人可能会选择为此类任务设计的带有国际化考量的更强大的语言或库。
