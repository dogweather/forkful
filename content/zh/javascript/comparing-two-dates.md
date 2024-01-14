---
title:                "Javascript: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 为什么

比较两个日期在Javascript中是一个常见的任务，因为它可以帮助我们判断日期是在之前还是之后，或者它们是否相同。它也可以用来排序日期和计算日期之间的差距。

## 如何进行

首先，我们需要创建两个日期对象，然后使用`getTime()`方法来获取日期的毫秒数。接下来，我们可以使用条件语句来比较这两个日期的毫秒数，例如：

```Javascript
// 创建两个日期对象
let date1 = new Date('2021-08-01');
let date2 = new Date('2021-08-15');

// 获取毫秒数
let date1ms = date1.getTime();
let date2ms = date2.getTime();

// 比较两个日期的毫秒数
if (date1ms > date2ms) {
  console.log(`${date1} 在 ${date2} 之后`);
} else if (date1ms < date2ms) {
  console.log(`${date1} 在 ${date2} 之前`);
} else {
  console.log(`${date1} 和 ${date2} 是相同的日期`);
}
```

以上代码的输出将是：`2021-08-01 在 2021-08-15 之前`。

我们也可以使用`getDate()`和`getMonth()`方法来获取日期的月份和日期，然后进行比较。例如：

```Javascript
// 获取月份和日期
let date1Month = date1.getMonth();
let date1Date = date1.getDate();
let date2Month = date2.getMonth();
let date2Date = date2.getDate();

// 比较日期和月份
if (date1Month > date2Month) {
  console.log(`${date1} 在 ${date2} 之后`);
} else if (date1Month < date2Month) {
  console.log(`${date1} 在 ${date2} 之前`);
} else {
  // 如果月份相同，则比较日期
  if (date1Date > date2Date) {
    console.log(`${date1} 在 ${date2} 之后`);
  } else if (date1Date < date2Date) {
    console.log(`${date1} 在 ${date2} 之前`);
  } else {
    console.log(`${date1} 和 ${date2} 是相同的日期`);
  }
}
```

以上代码的输出也将是：`2021-08-01 在 2021-08-15 之前`。

## 深入了解

在Javascript中，日期可以表示为毫秒数，也可以使用内置的日期对象来进行操作。我们也可以使用日期对象的方法来比较两个日期，例如使用`getTime()`方法来获取日期的毫秒数，使用`getDate()`和`getMonth()`方法来获取日期的日期和月份。

## 参考链接

- [JavaScript Date 对象](https://www.runoob.com/jsref/jsref-obj-date.html)
- [Date.prototype.getTime()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime)
- [日期对象](https://www.w3school.com.cn/js/jsref_obj_date.asp)
- [JavaScript 日期比较](https://www.runoob.com/w3cnote/javascript-date-compare.html) 

## 查看也能

- [JavaScript 比较日期的多种方式](https://segmentfault.com/a/1190000037614741)
- [JavaScript 中的日期比较](https://www.cnblogs.com/shang-chi/p/13542160.html)
- [JavaScript 比较两个日期的方法](https://www.codetd.com/article/13441915)