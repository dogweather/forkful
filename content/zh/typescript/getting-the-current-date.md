---
title:                "获取当前日期。"
html_title:           "TypeScript: 获取当前日期。"
simple_title:         "获取当前日期。"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 是什么 & 为什么?
获取当前日期是指在程序中获取当天的日期，通常用于记录日志或设置定时任务。程序员经常这样做是因为日期是程序开发中不可或缺的一个信息，它可以帮助我们正确地追踪和记录数据。

## 如何:
```TypeScript
const currentDate: Date = new Date();
console.log(currentDate.toLocaleDateString()); // 2021/07/26
console.log(currentDate.toLocaleTimeString()); // 10:05:00 AM
```

## 深入探讨:
1. 历史背景: 在过去，获取当前日期需要使用多种不同的函数和方法来处理，但随着技术的发展，现在我们可以使用更简单的方法来获取日期。
2. 其他选择: 在TypeScript中，除了使用Date对象来获取当前日期外，还可以使用moment.js库来处理日期。
3. 实现细节: Date对象中有多种方法可以获取当前日期，如toLocaleDateString()和toLocaleTimeString()。可以根据自己的需求选择使用哪种方法。

## 查看更多:
- [TypeScript Date 类型文档](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-7.html#ecmascript-version-based-type-libraries)
- [Moment.js 官方文档](https://momentjs.com/docs/)