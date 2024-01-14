---
title:    "Javascript: 获取当前日期"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么

获取当前日期是在编写Javascript代码时经常会遇到的情况。它可以帮助我们在程序中使用日期和时间功能，并且很容易添加到我们的代码中。

## 如何实现

要获取当前日期，我们可以使用内置的Javascript `Date`对象。通过创建一个新的`Date`实例，我们可以获取当前的日期、月份、年份、小时数、分钟数、秒数、毫秒数等信息。

例如，要获取当前日期的年份，我们可以使用以下代码：

```Javascript
const currentDate = new Date();
const currentYear = currentDate.getFullYear();
console.log(currentYear); // 输出当前年份，如2021
```

同样地，要获取当前日期的月份，我们可以使用`getMonth()`方法，它会返回一个0到11的整数，代表一年中的月份。需要注意的是，0表示一月，11表示十二月。例如：

```Javascript
const currentMonth = currentDate.getMonth();
console.log(currentMonth); // 输出当前月份，如3（代表4月）
```

## 深入了解

虽然获取当前日期看起来很简单，但其背后的原理并不简单。Javascript的内置`Date`对象根据本地时间来表示日期和时间，而本地时间在不同的时区和时间设置下是不同的。另外，获取当前日期只能返回当前时刻，无法精确到毫秒数，如果需要精确到毫秒数，需要使用额外的库进行处理。

此外，`Date`对象也提供了许多其他有用的方法来获取特定日期、设置日期和时间等功能。如果想要更深入地了解`Date`对象的用法，可以参考官方文档或者其他教程。

## 参考链接

- [Javascript Date对象文档](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [获取日期的其他方法](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [Moment.js库，用于更精确地处理日期和时间](https://momentjs.com/)
- [阮一峰的Javascript教程](https://wangdoc.com/javascript/) （涵盖了Javascript的基础知识和常用方法）。

## 参见

- [在Javascript中格式化日期和时间](https://example.com)
- [如何在网页中显示当前日期和时间](https://example.com)
- [Javascript如何处理不同的时间和时区](https://example.com)