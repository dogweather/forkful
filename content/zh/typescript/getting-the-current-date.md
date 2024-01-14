---
title:    "TypeScript: 获取当前日期"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么要获取当前日期？

在软件开发过程中，获取当前日期是一个十分常见的需求。它可以用于记录数据的时间戳、计算时间差、以及其他与日期相关的功能。同时，获取当前日期也可以帮助我们更好地掌握时间，规划工作安排。

## 如何通过TypeScript获取当前日期？

获取当前日期的方法在不同的编程语言中会有所不同，而在TypeScript中，我们可以通过内置的Date类来实现。下面是一个代码示例：

```TypeScript
// 创建一个Date对象，获取当前时间
const currentDate = new Date();

// 获取年份
const currentYear = currentDate.getFullYear();
// 获取月份
const currentMonth = currentDate.getMonth() + 1; // 注意月份是从0开始计数，所以需要加1
// 获取日期
const currentDay = currentDate.getDate();

// 将日期输出为字符串
console.log(`今天是${currentYear}年${currentMonth}月${currentDay}日。`);
```

运行以上代码，你将会得到类似于`今天是2021年8月17日。`的输出结果。

## 深入了解：获取当前日期的更多方法

除了使用内置的Date类之外，我们还可以使用第三方库momentjs来更加灵活地获取当前日期。同时，我们也可以通过各种方法来格式化日期的输出，例如增加时区、调整日期格式等等。值得一提的是，在Node.js环境下，我们可以直接使用`new Date()`来获取当前日期，无需额外的设置。

## 看看这些链接！

如果你想了解更多关于获取当前日期的方法，可以参考以下链接：

- [TypeScript手册中的Date类介绍](https://www.typescriptlang.org/docs/handbook/2/classes.html#the-date-class)
- [Moment.js官方文档](https://momentjs.com/docs/)
- [在Node.js中获取当前日期的方法](https://stackoverflow.com/questions/17144252/getting-the-current-date-time-in-node-js/17143678)