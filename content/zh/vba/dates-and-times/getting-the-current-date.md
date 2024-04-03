---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:35.734695-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5728VBA\u4E2D\uFF0C\u4F7F\u7528`Date`\u51FD\
  \u6570\u53EF\u76F4\u63A5\u83B7\u53D6\u5F53\u524D\u65E5\u671F\uFF0C\u800C\u4F7F\u7528\
  `Now`\u51FD\u6570\u5219\u53EF\u83B7\u53D6\u5F53\u524D\u7684\u65E5\u671F\u548C\u65F6\
  \u95F4\u3002\u4E0B\u9762\u662F\u5982\u4F55\u64CD\u4F5C\u8FD9\u4E24\u8005\u7684\u65B9\
  \u6CD5\uFF1A."
lastmod: '2024-03-13T22:44:47.586459-06:00'
model: gpt-4-0125-preview
summary: "\u5728VBA\u4E2D\uFF0C\u4F7F\u7528`Date`\u51FD\u6570\u53EF\u76F4\u63A5\u83B7\
  \u53D6\u5F53\u524D\u65E5\u671F\uFF0C\u800C\u4F7F\u7528`Now`\u51FD\u6570\u5219\u53EF\
  \u83B7\u53D6\u5F53\u524D\u7684\u65E5\u671F\u548C\u65F6\u95F4\u3002\u4E0B\u9762\u662F\
  \u5982\u4F55\u64CD\u4F5C\u8FD9\u4E24\u8005\u7684\u65B9\u6CD5\uFF1A."
title: "\u83B7\u53D6\u5F53\u524D\u65E5\u671F"
weight: 29
---

## 如何做：
在VBA中，使用`Date`函数可直接获取当前日期，而使用`Now`函数则可获取当前的日期和时间。下面是如何操作这两者的方法：

```vb
Sub GetCurrentDate()
    ' 使用Date函数获取当前日期
    Dim currentDate As Date
    currentDate = Date
    Debug.Print "当前日期: "; currentDate
    
    ' 使用Now函数获取当前的日期和时间
    Dim currentDateTime As Date
    currentDateTime = Now
    Debug.Print "当前日期和时间: "; currentDateTime
End Sub
```

当你运行这个宏时，`Debug.Print`方法会将当前日期和当前日期及时间输出到VBA编辑器中的即时窗口。例如：

```
当前日期: 2023/4/12
当前日期和时间: 2023/4/12 3:45:22 PM
```

请记住，日期格式可能会根据用户计算机的系统设置而有所不同。

## 深入了解
`Date`和`Now`函数封装了在Visual Basic for Applications中处理日期和时间的复杂性，提供了一个应用程序级别的抽象，使得处理日期变得简单直观。历史上，编程中处理日期和时间充满了挑战，包括处理不同的时区、夏令时变化以及各种日期格式。

在VBA中，这些函数依赖于底层系统的日期和时间，这意味着它们受到用户的区域设置和系统设置的影响。这既确保了与用户环境的一致性，也意味着在全球应用程序中需要仔细处理本地化和时区调整。

尽管对于许多应用程序来说，尤其是在Office自动化的范围内，VBA的日期和时间功能完全适用，但它们可能缺乏处理更复杂应用程序（如高频交易系统或科学模拟）所需的精确性或细粒度。在这种情况下，其他编程环境或语言，如Python或C#，可能提供更复杂的日期和时间操作库。

尽管如此，对于在Excel、Word或其他Office应用程序的上下文中涉及日期和时间的绝大多数任务来说，VBA的`Date`和`Now`功能提供了难以击败的简单性、性能和易用性平衡。
