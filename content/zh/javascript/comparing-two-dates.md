---
title:                "比较两个日期"
html_title:           "Javascript: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

比较两个日期是指将两个日期进行对比，以确定它们是否相等、哪一个日期更早或者更晚。程序员通常会进行这样的比较来处理日期数据，以便在应用程序中正确地排序和处理日期。

## 怎样：

Javascript中可以使用Date对象来比较两个日期。日期对象有一个getTime()方法，它会返回日期的时间戳，也就是从1970年1月1日开始经过的毫秒数。通过比较两个日期对象的时间戳，就可以确定它们的先后顺序。

```Javascript
let date1 = new Date("2021-01-01");
let date2 = new Date("2021-02-01");

if (date1.getTime() > date2.getTime()) {
  console.log("date1 is after date2");
} else if (date1.getTime() < date2.getTime()) {
  console.log("date1 is before date2");
} else {
  console.log("date1 is equal to date2");
}
```

输出：

```
date1 is before date2
```

## 深入了解：

在历史上，人们使用不同的日历系统来记录日期，而不同的日历系统可能会导致不同的日期比较结果。除了使用时间戳来比较日期，程序员也可以使用库或框架中提供的方法来处理日期。例如，Moment.js是一个流行的Javascript日期处理库，它提供了丰富的日期比较和操作功能。

## 参考链接：

- [Javascript Date 对象](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js官方网站](https://momentjs.com/)