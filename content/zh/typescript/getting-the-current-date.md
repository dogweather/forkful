---
title:    "TypeScript: 获取当前日期"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 为什么要获取当前日期

不管是在日常生活还是在编程中，获取当前日期都是一项常见的任务。在日常生活中，人们可能会想知道今天是星期几或者日期是几号，而在编程中，获取当前日期也是一项基础的操作。通过获取当前日期，我们可以实现一些与日期有关的功能，比如创建日历、计算日期差值等。接下来，我们将用 TypeScript 来讲解如何获取当前日期。

## 如何实现

要获取当前日期，我们可以使用 TypeScript 内置的 `Date` 对象。`Date` 对象可以创建一个包含当前日期和时间的实例。我们可以通过创建一个新的 Date 对象来获取当前日期的相关信息，如下所示：

```TypeScript
const currentDate = new Date();
console.log(currentDate);
```

上面的代码会输出一个包含当前日期和时间的 `Date` 对象。但是，如果我们只想获取日期部分，我们可以使用 `getDate()`、`getMonth()`、`getFullYear()` 和 `getDay()` 方法来分别获取日期的日、月、年和星期几。

```TypeScript
const currentDate = new Date();
const day = currentDate.getDate(); // 获取当天日期
const month = currentDate.getMonth() + 1; // 月份从 0 开始，所以需要加 1
const year = currentDate.getFullYear(); // 获取完整年份
const weekDay = currentDate.getDay(); // 获取星期几，0代表星期日，1代表星期一，以此类推

console.log(`今天是${month}月${day}日，星期${weekDay}，${year}年。`);
```

以上代码会输出类似于 `今天是10月29日，星期四，2020年。` 的结果。

## 深入了解

除了获取当前日期外，`Date` 对象还可以实现一些其他的日期操作，如设置日期、计算日期差值等。我们可以通过 `setDate()`、`setMonth()`、`setFullYear()` 等方法来设置日期，通过 `getTime()` 方法来获取距离 1970 年 1 月 1 日之间的毫秒数，从而可以方便地计算日期差值。有关 `Date` 对象的更多信息，可以查看官方文档进行深入了解。

## 参考链接

- [TypeScript 官方文档- Date](https://www.typescriptlang.org/docs/handbook/standard-library.html#date)
- [JavaScript中的Date对象详解](https://juejin.im/post/5a2012c6f265da431a42b468)
- [深入了解 JavaScript 的Date对象](https://juejin.im/post/5ba93314f265da0ab7191103)

## 参见

- [TypeScript 官方文档](https://www.typescriptlang.org/docs/)
- [TypeScript 入门教程](https://www.typescriptlang.org/docs/handbook/typescript-in-5-minutes.html)