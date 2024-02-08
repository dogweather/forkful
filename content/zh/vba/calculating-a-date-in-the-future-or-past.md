---
title:                "计算未来或过去的日期"
aliases:
- zh/vba/calculating-a-date-in-the-future-or-past.md
date:                  2024-02-01T21:48:48.857672-07:00
model:                 gpt-4-0125-preview
simple_title:         "计算未来或过去的日期"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/vba/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？
计算未来或过去的日期涉及到确定一个与给定日期相隔特定天数、月数或年数的日期。程序员经常需要这项功能来自动化提醒、订阅、到期日期和各种应用程序中的计划任务。

## 如何操作：
在Visual Basic for Applications (VBA)中，用于计算未来或过去日期的主要函数是`DateAdd()`。此函数将指定的时间间隔加到一个日期上，返回一个新日期。

这里有一个基础示例，将当前日期增加10天：

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' 给当前日期增加10天
Debug.Print futureDate ' 输出类似于：04/20/2023
```

类似地，要找到10天前的日期：

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' 从当前日期减去10天
Debug.Print pastDate ' 输出：03/31/2023，假设今天是04/10/2023
```

这些示例非常直接。你可以用其他间隔代码替换`"d"`，例如`"m"`代表月份，`"yyyy"`代表年份，来计算不同类型的日期计算。这里是怎样计算一年后的日期：

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' 给当前日期增加1年
Debug.Print nextYear ' 输出：04/10/2024，如果今天是04/10/2023
```

## 深入了解
`DateAdd`函数自VBA诞生以来一直是其基础组成部分，源自其前身BASIC。尽管它为从日期中添加或减去时间间隔提供了简易性，但重要的是要注意，包括日期处理功能在内的VBA，可能并不总是能达到新编程语言所提供的便利性或高效性。

例如，使用`datetime`模块的现代语言Python或使用类似`moment.js`和`date-fns`等库的JavaScript提供了更直观且强大的日期操作方式。这些选项为本地化、时区和闰年提供了更好的支持，这可能使它们更适合需要在全球范围内进行精确日期计算的应用程序。

然而，对于需要在Microsoft Office生态系统内集成的Excel宏和应用程序而言，VBA仍然是一个实用的选择。直接访问和操作Excel数据的简便性是一个重大优势。此外，对于大多数基本日期计算，如计划和提醒，VBA中的`DateAdd()`提供了一个足够的简单解决方案。它的语法易于新手掌握，而其整合到更广泛的Office套件应用程序中确保了其在特定用例中的相关性。

总之，虽然其他编程语言可能提供了更现代化的日期计算方法，但VBA中的`DateAdd()`证明了该语言在最需要它的领域中的持久力。
