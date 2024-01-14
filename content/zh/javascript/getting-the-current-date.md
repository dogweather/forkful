---
title:                "Javascript: 获取当前日期"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

为什么: 获取当前日期是编程中经常需要的一个操作。无论你是在开发一个网页还是在创建一个应用程序，都会涉及到与日期相关的功能。因此，了解如何获取当前日期是非常重要的。

## 如何做

为了获取当前日期，我们需要使用内置的Date对象。这个对象允许我们访问和操作日期和时间。让我们来看一个简单的例子：

``` javascript
// 创建一个新的Date对象
let currentDate = new Date();

// 打印当前日期
console.log(currentDate);

// Output: Fri Apr 02 2021 10:24:35 GMT+0800 (China Standard Time)
```

在上面的代码中，我们首先使用`new Date()`来创建一个新的Date对象，该对象将保存当前日期和时间。然后，我们使用`console.log()`来打印出当前日期。你也可以看到输出结果中包含的时区信息，这是因为我们使用的是中国标准时间。如果你想要获取的是UTC时间，可以使用`getUTCDate()`方法来替代`getDate()`方法。

除了打印日期外，我们还可以获取特定的日期信息，例如年份、月份、日期等。让我们来看一个例子：

``` javascript
// 获取当前年份
let currentYear = currentDate.getFullYear();
console.log(currentYear);

// Output: 2021

// 获取当前月份（从0开始，所以需要加1）
let currentMonth = currentDate.getMonth() + 1;
console.log(currentMonth);

// Output: 4

// 获取当前日期
let currentDay = currentDate.getDate();
console.log(currentDay);

// Output: 2
```

除了以上日期信息外，我们还可以获取更多的数据，例如小时、分钟、秒等。你可以在[MDN文档](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)中查看所有可用的方法。

## 深入了解

在深入了解获取当前日期之前，我们先来谈谈日期格式。在国际标准中，日期格式通常为`年-月-日`，例如`2021-04-02`。然而，在不同的国家和文化中，日期格式可能会有所不同。在JavaScript中，我们可以使用`toLocaleDateString()`方法来获取适合特定语言环境的日期格式，例如：

``` javascript
let currentDateString = currentDate.toLocaleDateString('zh-CN');

// Output: 2021/4/2
```

另外，如果你需要将日期格式化为特定的样式，可以使用第三方库，例如[Day.js](https://day.js.org/)或[Moment.js](https://momentjs.com/)。这些库提供了更多的灵活性和功能，可以满足不同的需求。

除了获取当前日期，我们还可以对日期进行操作。比如，我们可以增加或减少特定的时间，可以比较两个日期的差异，可以判断一个日期是否在另一个日期之后等等。这对于处理日期相关的数据非常有用，因此建议你在学习日期操作的同时，也要了解如何避免常见的日期错误。

## 参考链接

- [MDN文档 - Date对象](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Day.js官方网站](https://day.js.org/)
- [Moment.js官方网站](https://momentjs.com/)
- [避免日期相关错误的建议](https://codeburst.io/browserify-vs-webpack-b3d7ca08a0e9)