---
title:                "计算未来或过去的日期"
html_title:           "TypeScript: 计算未来或过去的日期"
simple_title:         "计算未来或过去的日期"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

##为什么

有时候我们需要计算未来或过去的日期，比如计划一次旅行或者预订特定的会议日期。使用TypeScript编程语言可以轻松地实现这一需求，并且保证计算结果的准确性。

##如何做

计算日期的思路是通过日期对象(Date object)来表示日期，并使用内置的方法来计算。首先，我们需要确定计算的日期是未来的还是过去的，然后确定计算的时间单位（如天、月、年），最后以该单位使用日期对象的方法进行计算，并输出结果。

###在未来计算日期

首先，我们需要创建一个新的日期对象来表示当前日期。我们可以使用`new Date()`来实现。接着，我们可以使用`setFullYear()`、`setMonth()`、`setDate()`等方法来设置需要计算的未来日期。最后，我们通过使用`toLocaleDateString()`方法来将结果输出为字符串。

```TypeScript
let currentDate: Date = new Date();
currentDate.setFullYear(currentDate.getFullYear() + 1); //计算未来一年后的日期
currentDate.setMonth(currentDate.getMonth() + 1); //计算未来一个月后的日期
currentDate.setDate(currentDate.getDate() + 1); //计算未来一天后的日期

console.log(currentDate.toLocaleDateString()); //输出未来日期
```

输出结果为：`9/25/2021`，表示未来一年一月一天后的日期为2021年9月25日。

###在过去计算日期

在计算过去日期时，使用的方法和步骤与计算未来日期类似。唯一的区别在于将日期减去相应的时间单位，如`getFullYear()`改为`setFullYear(currentDate.getFullYear() - 1)`。同样地，最后使用`toLocaleDateString()`方法将结果输出。

```TypeScript
let currentDate: Date = new Date();
currentDate.setFullYear(currentDate.getFullYear() - 1); //计算过去一年前的日期
currentDate.setMonth(currentDate.getMonth() - 1); //计算过去一个月前的日期
currentDate.setDate(currentDate.getDate() - 1); //计算过去一天前的日期

console.log(currentDate.toLocaleDateString()); //输出过去日期
```

输出结果为：`9/23/2019`，表示过去一年一月一天前的日期为2019年9月23日。

##深入探究

在TypeScript中，日期对象是基于国际标准时间（UTC）来存储和运算的。这意味着通过使用内置的方法，我们可以轻松地实现跨时区的日期计算。另外，TypeScript也提供了许多其他的日期方法，如`getTime()`、`getDay()`等来满足不同的日期计算需求。使用这些方法可以更加灵活和高效地计算日期。

##参考链接

- [TypeScript官方文档](https://www.typescriptlang.org/docs)
- [深入理解JavaScript中的日期和时间](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [阮一峰的ES6教程-日期的扩展](https://es6.ruanyifeng.com/#docs/date)

##更多阅读

如果你对日期计算感兴趣，可以继续学习如何处理不同的时间格式和时区，以及如何处理闰年和时区差异等问题。通过学习更多的日期相关知识，你可以提升自己的编程能力，并在工作中实现更多复杂的日期计算需求。