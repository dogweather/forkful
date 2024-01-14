---
title:                "Javascript: 未来或过去计算日期"
simple_title:         "未来或过去计算日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 为什么 

在编程中，我们经常需要计算未来或过去的日期。比如，在日历应用程序中，我们可能需要计算下一个月的日期，或者在计划应用程序中，我们可能需要计算几天之后的日期。通过编程来计算日期，可以帮助我们更加方便和准确地管理时间。

## 如何操作

为了计算未来或者过去的日期，我们可以使用JavaScript中的内置日期对象Date()。首先，我们需要创建一个新的Date对象，然后使用getDate()方法来获得当前日期的天数。接下来，我们可以使用setDate()方法来设置新的日期值，然后使用toLocaleDateString()方法来格式化我们需要的日期格式。下面是一个示例代码：

```Javascript
//创建新的Date对象
let currentDate = new Date();

//获取当前日期的天数
let currentDay = currentDate.getDate();

//设置未来的日期，示例为明天
currentDate.setDate(currentDay + 1);

//使用toLocaleDateString()方法来格式化日期
let futureDate = currentDate.toLocaleDateString();

//控制台输出未来的日期
console.log(futureDate); //输出明天的日期
```

输出：`5/4/2021`

## 深入探讨

除了使用内置的Date对象来计算日期，我们还可以使用第三方库来更加灵活地管理日期。比如，Moment.js是一个很受欢迎的JavaScript库，它提供了一系列易于使用的方法来操作日期和时间。通过使用Moment.js，我们可以轻松地计算未来或者过去的日期，并且还可以使用各种不同的日期格式。下面是一个使用Moment.js来计算明天日期的示例代码：

```Javascript
//引入Moment.js库
const moment = require('moment');

//使用add()方法来增加一天
let tomorrow = moment().add(1, 'days').format('MM/DD/YYYY');

//控制台输出明天的日期
console.log(tomorrow); //输出明天的日期
```

输出：`05/04/2021`

## 参考链接

- [JavaScript内置Date对象文档](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js官方网站](https://momentjs.com/)
- [Moment.js文档](https://momentjs.com/docs/)
- [一起来学习Javascript中的日期对象吧](https://juejin.cn/post/6844903736717620238)