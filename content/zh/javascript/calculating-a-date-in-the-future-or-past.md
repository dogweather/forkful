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

## 什么是时间计算 & 为什么要计算时间？

时间计算是一种编程技术，通过编码来测量未来或过去的日期。程序员经常需要计算时间，因为它可以帮助他们准确地跟踪事件，创建日程安排以及进行数据分析。

## 如何进行时间计算：

在Javascript中，您可以使用内置的```Date```对象来进行时间计算。以下是计算一天后的日期的示例代码：

```Javascript
let currentDate = new Date();
let tomorrow = new Date(currentDate.setDate(currentDate.getDate() + 1));
console.log(tomorrow);
// Output: Wed Mar 24 2021 13:21:27 GMT+0800 (China Standard Time)
```

可以使用类似的方法来计算过去的日期，只需要将偏移量改为负数。

## 深入了解：

时间计算在历史上具有很重要的作用。在早期的计算机系统中，日期和时间的计算是很复杂的，需要通过编码来进行，而不是像今天这样使用内置函数。

除了使用内置的```Date```对象，还有许多库提供了更方便和精确的时间计算方法，如moment.js和dayjs。您可以根据自己的需要选择适合的方法。

实际上，无论使用什么方法，时间计算本质上都是通过将日期转换为时间戳（表示从某个固定日期开始的秒数）来实现的，然后将偏移量添加或减去特定的秒数来得到新的日期。

## 参考链接：

了解更多关于时间计算的知识，可以参考以下链接：

- [Javascript官方文档](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js官网](https://momentjs.com/)
- [Day.js官网](https://day.js.org/)