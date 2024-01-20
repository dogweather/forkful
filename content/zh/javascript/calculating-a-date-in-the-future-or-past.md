---
title:                "计算未来或过去的日期"
html_title:           "Javascript: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 什么以及为什么？(What & Why?)

在JavaScript编程中，我们常常需要计算未来或过去的某一日期（计算相对当前时间向前或向后移动一定的单位时间得到的日期）。这对于处理如日程安排、提醒及定时任务等功能尤其重要。

## 如何做：(How to:)

在JavaScript中，我们可以通过`getDate`、`setDate`等方法来完成相关操作。以下是一些简单的示例：

```JavaScript
var oneWeekFromNow = new Date();
oneWeekFromNow.setDate(oneWeekFromNow.getDate() + 7); // 设置为7天后的日期
console.log(oneWeekFromNow); // 输出7天后的日期

var threeDaysAgo = new Date();
threeDaysAgo.setDate(threeDaysAgo.getDate() - 3); // 设置为3天前的日期
console.log(threeDaysAgo); // 输出3天前的日期
```
在输出示例中，你会看到新的日期，分别表示一周后以及三天前的日期。

## 深入研究（Deep Dive）

JavaScript Date对象自1995年发布以来，一直是处理日期和时间的主要工具。然而，有一些扩展库如moment.js和date-fns也提供了更方便的API和更强大的功能。

在JavaScript中调整日期，我们可以调整年份、月份、小时等各个维度，上述示例仅展示了天数调整的基础用法。

计算未来或过去的日期本质上是对日期对象进行加法或减法运算，这就是为什么我们可以用加法和减法来设置未来或过去的日期。

## 另请参阅 (See Also)

以下是一些有助于您深入理解日期处理的有用链接：

1. [MDN JavaScript Date Reference](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js库](https://momentjs.com/)
3. [Date-fns库](https://date-fns.org/)