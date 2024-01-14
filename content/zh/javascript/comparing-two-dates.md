---
title:                "Javascript: 比较两个日期"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么要比较两个日期？

日期是编程中非常常见的数据类型，我们经常需要比较两个日期的大小或者是否相等。比如在制作日历应用或者计算剩余天数的功能中，就需要用到比较日期的技巧。在本文中，我们将介绍如何使用Javascript来比较两个日期，并且深入探讨背后的原理。

## 如何做？

比较两个日期的方法有很多种，但是在Javascript中，我们可以使用Date对象和内置的比较函数来实现。

首先，我们需要创建两个Date对象，分别表示要比较的两个日期。假设我们要比较的日期是2020年6月1日和2020年6月2日，我们可以这样创建对象：

```Javascript
let date1 = new Date(2020, 6, 1);
let date2 = new Date(2020, 6, 2);
```

接下来，我们可以使用内置的比较函数来比较两个日期的大小。比如，我们可以使用`getTime()`方法来获取日期的毫秒数，然后使用普通的比较运算符来比较：

```Javascript
if (date1.getTime() > date2.getTime()) {
  console.log("date1 is later than date2");
} else if (date1.getTime() < date2.getTime()) {
  console.log("date2 is later than date1");
} else {
  console.log("date1 and date2 are equal");
}
```

上面的代码会输出"date2 is later than date1"，因为日期2比日期1晚一天。

我们也可以使用`getDate()`方法来比较两个日期的具体日期是不是相同：

```Javascript
if (date1.getDate() === date2.getDate()) {
  console.log("date1 and date2 are on the same date");
} else {
  console.log("date1 and date2 are on different dates");
}
```

这段代码会输出"date1 and date2 are on different dates"，因为这两个日期的具体日期不同。

## 深入探讨

上面的例子只是最基本的日期比较方法，实际上，在计算机中，日期并不是简单的数字，而是一个复杂的对象。这个对象包括很多属性和方法，比如年份、月份、日期、小时、分钟、秒等等。

在Javascript中，我们使用的是标准的格里高利历，它是一种公历，以前的日期对象共有18个属性，但是新版的标准又增加了三个属性。了解这些属性可以帮助我们更好地理解日期比较的原理，并且可以在日期计算中发挥更大的作用。

## 参考链接

- [MDN: Date](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [知乎: 如何比较日期？](https://www.zhihu.com/question/26655114)
- [W3Schools: JavaScript Date 对象](https://www.w3schools.com/js/js_dates.asp)

# 另请参阅

- [Node.js的日期比较方法](https://nodejs.org/api/datetime.html#datetime_datetimemath_greater_than_date1_date2)
- [使用Moment.js进行日期比较](https://momentjs.com/docs/#/query/is-same/)