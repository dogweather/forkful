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

## 是什么 & 为什么?

计算未来或过去的日期是在当前日期基础上增加或减少一定的时间单位，如天、周或月。程序员经常需要使用它们来实现日程表、记事功能、流程控制等功能。

## 怎么做:

在 TypeScript 中操作日期是非常简单的。让我们来看一些计算未来和过去日期的例子。

```TypeScript
let date = new Date();

// 加7天
date.setDate(date.getDate() + 7);
console.log(date);

// 减30天
date.setDate(date.getDate() - 30);
console.log(date);

// 加1年
date.setFullYear(date.getFullYear() + 1);
console.log(date);
```
示例输出为：
```TypeScript
2023-8-31T08:00:00.000Z
2023-8-1T08:00:00.000Z
2024-8-1T08:00:00.000Z
```

## 深度探索:

1. 历史背景: 在计算机的早期阶段，日期和时间的处理需要做很多繁琐的工作。这是因为每一种语言、操作系统甚至应用程序都有自己的日期和时间格式。但随着 JavaScript （及其超集 TypeScript）的流行，日期和时间的操作变得更为简单和一致。

2. 可选方法: 除了使用原生的 `Date` 对象，还有许多库，如Moment.js、Date-fns和Day.js，可以提供更加强大和灵活的日期/时间操作。

3. 实现细节: 在JavaScript和TypeScript中，日期是以毫秒存储的，自1970年1月1日00:00:00 UTC (协调世界时间)以来的时间。所有的日期和时间计算都基于这个事实进行。

## 参考资料:

- MDN Web Docs (JavaScript Date 对象): [链接](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)

- Moment.js: [链接](https://momentjs.com/)

- Date-fns: [链接](https://date-fns.org/)

- Day.js: [链接](https://day.js.org/)