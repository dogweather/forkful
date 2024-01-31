---
title:                "获取当前日期"
date:                  2024-01-20T15:15:36.848814-07:00
html_title:           "Bash: 获取当前日期"
simple_title:         "获取当前日期"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
获取当前日期是读取和表示系统当前日期和时间的过程。编程中这么做是为了记录事件发生的时间点、计算日期差或简单地展示给用户。

## 如何：
```Javascript
// 获取当前日期和时间
const now = new Date();
console.log(now.toString()); // 示例输出：Wed Apr 05 2023 21:15:07 GMT+0800 (China Standard Time)

// 获取今天的日期信息
console.log(now.getDate()); // 示例输出：5
console.log(now.getMonth() + 1); // 示例输出：4，月份是从0开始计数的
console.log(now.getFullYear()); // 示例输出：2023
```

## 深入了解
JavaScript 中使用 `Date` 对象来处理日期和时间是从其诞生之初就开始的。简单的 `new Date()` 调用能够给我们系统的当前时间。事实上，你还可以使用 `Date.now()` 获取自1970年1月1日以来的毫秒数，这也是 Unix 时间戳的起点。我们之所以需要注意月份索引从0开始，这是历史悠久的设计决策，1月是0，12月是11。

除了 `Date`，JavaScript 也允许使用库像 Moment.js 或 date-fns 来处理更复杂的日期问题，比如格式化和时区转换。这些库提供了更多功能和更好的兼容性，但随着 `Temporal`—一个现代日期时间API逐渐成熟，我们将享有一个更强大的原生功能。

`Date` 实现细节方面，JavaScript 的 `Date` 对象是基于时间戳构建的，时间戳表示自Unix纪元以来的毫秒数。这意味着，即使你在代码里没有明确指定时区，`Date` 对象也会依据你的系统时区来展示日期和时间。

## 参考链接
- MDN Web Docs 关于 Date 对象: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js 库: [https://momentjs.com/](https://momentjs.com/)
- date-fns 库: [https://date-fns.org/](https://date-fns.org/)
- 提案中的 Temporal API: [https://tc39.es/proposal-temporal/docs/index.html](https://tc39.es/proposal-temporal/docs/index.html)
