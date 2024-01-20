---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么与为什么？
从字符串解析日期是将日期格式的文本转换为可在编程中使用的日期和时间类型。程序员之所以这样做，是因为这样可以让我们方便地从多种数据源（如用户输入或文件）获取日期信息，并以程序需要的方式使用和处理这些日期。

## 如何做：
在C#中解析日期字符串需要使用DateTime类的Parse方法或TryParse方法。下面的代码展示了如何进行操作：

```C#
using System;

string dateString = "2022/07/30";
DateTime date = DateTime.Parse(dateString);

Console.WriteLine(date);
```

上述代码将打印出：2022年7月30日 0:00:00

如果输入的日期字符串格式不正确或者非法，`Parse`方法会抛出异常。在这种情况下，我们可以使用`TryParse`方法，它不会抛出异常，而是返回一个布尔值来表示转换是否成功：

```C#
using System;

string dateString = "2022/02/30";
DateTime date;
bool success = DateTime.TryParse(dateString, out date);

Console.WriteLine(success ? date.ToString() : "Invalid date");

```
这段代码将打印出："Invalid date"，因为2022年的2月没有30日。

## 深入探讨
在早期的.NET版本中，日期的解析要复杂得多，需要手动处理诸如不同区域设置的日期格式问题。现在，我们有了DateTime.Parse方法，使其变得更为简单。然而，除了Parse和TryParse，还有一些其他的解析方法，比如ParseExact和TryParseExact，它们允许你指定日期字符串的精确格式。

当你处理的日期字符串格式不确定，或者你需要兼容不同的日期格式时，TryParse可能是一个更好的选择。另一方面，如果你知道输入的日期字符串格式总是相同的，那么Parse或ParseExact可能更为高效。

## 另请参阅
* MSDN上的`DateTime.Parse`方法的解释 [链接](https://msdn.microsoft.com/library/cc165448.aspx)
* MSDN上的`DateTime.TryParse`方法的解释 [链接](https://msdn.microsoft.com/library/9h21f14e.aspx)
* Stack Overflow上关于如何解析日期字符串的问题和答案 [链接](https://stackoverflow.com/questions/919244/converting-a-string-to-datetime)