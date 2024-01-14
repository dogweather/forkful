---
title:    "Elixir: 比较两个日期"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

为什么：比较两个日期有什么用？

比较两个日期在日常编程中是非常常见的。它可以帮助我们确定两个日期的先后顺序，或者计算出两个日期之间的时间差。举个例子，如果你正在开发一个日程安排应用程序，你可能需要比较用户选择的日期，以便将事件按照正确的顺序显示。在这篇博客文章中，我们将学习如何使用Elixir编程语言来比较两个日期。

## 如何：

我们将首先使用Elixir的`DateTime`模块来创建两个日期对象，并将它们分别存储在`date_1`和`date_2`变量中：

```Elixir
date_1 = DateTime.from_naive(~N[2020-01-01 00:00:00], "Etc/UTC")
date_2 = DateTime.from_naive(~N[2020-06-01 00:00:00], "Etc/UTC")
```

接下来，我们可以使用`DateTime.compare/2`函数来比较这两个日期，它将返回一个整数值，表示第一个日期相对于第二个日期的关系。如果第一个日期早于第二个日期，返回值将小于0；如果两个日期相同，返回值将等于0；如果第一个日期晚于第二个日期，返回值将大于0。让我们来看看下面的例子：

```Elixir
DateTime.compare(date_1, date_2) # 返回-1
DateTime.compare(date_2, date_1) # 返回1
DateTime.compare(date_1, date_1) # 返回0
```

除了使用`DateTime.compare/2`函数，我们还可以使用`Date.compare/2`和`Time.compare/2`来分别比较日期和时间。

## 深入探讨：

值得注意的是，Elixir中的日期和时间类型都是不可变的。这意味着比较操作并不会改变原始的日期对象，而是返回一个新的日期对象。因此，在比较日期之后，如果你想要修改日期对象，你需要将返回的新日期对象重新赋值给原始变量。

此外，Elixir还提供了其他一些用于比较日期和时间的函数，比如`DateTime.before?/2`、`DateTime.after?/2`和`DateTime.diff/2`，它们分别用于检查一个日期是否早于/晚于另一个日期，并计算两个日期之间的时间差。

希望本文能帮助你了解如何在Elixir中比较两个日期。要了解更多关于日期和时间类型的信息，请查看[Elixir官方文档](https://hexdocs.pm/elixir/DateTime.html)。

## 参考链接：

- [Elixir官方文档](https://hexdocs.pm/elixir/DateTime.html)