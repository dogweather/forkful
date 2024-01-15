---
title:                "计算未来或过去日期"
html_title:           "Javascript: 计算未来或过去日期"
simple_title:         "计算未来或过去日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么

计算未来或过去日期在编程中是一个常见的需求。它可以帮助我们在应用程序中进行时间计算和跟踪，从而使我们的代码更加灵活和智能化。

## 如何操作

首先，我们需要使用Date对象来表示日期和时间。然后，我们可以使用内置的JavaScript方法来操作日期对象，如setDate()，setMonth()，setFullYear()等等。让我们看一个例子：

```Javascript
let now = new Date() // 创建一个代表当前日期和时间的Date对象
now.setDate(now.getDate() + 3) // 将日期调整为3天后
console.log(now) // 输出：Fri Oct 08 2021 15:56:01 GMT+0800 (中国标准时间)
```

为了计算一个日期之前的日期，我们可以使用负数作为参数。比如，```now.setDate(now.getDate() - 7)``` 将会把日期调整为7天前。

## 深入讨论

上面的示例只是一个开始，实际上，我们还可以使用更多的方法来进行日期计算。比如，我们可以使用setFullYear()和setMonth()来调整年份和月份，从而实现准确的日期计算。此外，我们还可以通过比较不同的日期对象来确定时间差，或者使用toDateString()方法来转换日期对象为可读的字符串。关于日期计算，还有很多你可以探索的功能，让我们一起继续学习吧！

## 查看更多

- [MDN 文档 - Date 对象](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3School 教程 - JavaScript 日期和时间](https://www.w3school.com.cn/js/js_date_methods.asp)
- [掘金社区 - JavaScript 中日期的操作](https://juejin.cn/post/6844904129337608205)