---
title:    "Gleam: 计算未来或过去的日期"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

为什么：为什么要学习如何计算未来或过去的日期，只需要简短的1-2句话。

## 为什么

日期计算在编程中是一个常见的需求。无论是订票网站、日程表应用程序还是电子商务网站，都可能需要计算未来或过去的日期。通过学习如何在Gleam中进行这样的计算，您可以轻松地完成这项任务。

## 如何做

在Gleam中，有两种常用的方式来计算未来或过去的日期。下面是使用代码示例和输出来演示这两种方式的方法。

### 计算未来日期

要计算未来的日期，您需要使用Gleam内置的日期库，并指定所需的天数。

```Gleam
import gleam/date

let future_date = date.add_days(date.now(), 7)
```

在这个例子中，我们使用了 `date.now()` 函数来获取当前日期，并使用 `date.add_days()` 函数来将当前日期的天数增加了 7 天。最终得到的 `future_date` 将是当前日期之后的第七天的日期。

### 计算过去日期

与计算未来日期类似，要计算过去的日期，您也需要使用 `date.add_days()` 函数。但是，这一次，您需要指定负数天数，来表示要减少的天数。

```Gleam
import gleam/date

let past_date = date.add_days(date.now(), -14)
```

在这个例子中，我们使用了 `-14` 来表示您想要在当前日期之前的第十四天的日期。通过这种方式，您可以轻松地计算过去的日期。

## 深入了解

除了 `date.add_days()` 函数之外，Gleam还提供了许多其他与日期相关的函数，包括 `date.add_months()`、`date.add_years()` 和 `date.add_weeks()`。通过熟练掌握这些函数，您可以更加灵活地进行日期计算。

## 参考资料

- [Gleam官方文档](https://gleam.run/documentation/)
- [Gleam日期库函数参考](https://gleam.run/documentation/stdlib/date/)

## 链接

[了解更多Gleam](https://gleam.run/) | [Gleam仓库](https://github.com/gleam-lang/gleam) | [Gleam社区论坛](https://elixirforum.com/c/other-languages/gleam)