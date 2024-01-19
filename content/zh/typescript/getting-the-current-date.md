---
title:                "获取当前日期"
html_title:           "Arduino: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
获取当前日期意味着让计算机返回此时此刻的日期和时间。程序员需要这个功能来记录事件发生的时间，对数据进行时间标记，或者是在特定时间触发事件。

## 如何操作：
在 TypeScript 中，我们可以使用 `Date.now() ` 函数或创建一个新的 `Date` 对象来获取当前日期和时间。代码如下：

```TypeScript
let now1 = Date.now();
console.log(now1);
// 输出：1628128650827 (时间戳)

let now2 = new Date();
console.log(now2);
// 输出：2021-08-05T02:30:50.827Z (UTC 格式的时间)
```
## 深入了解
(1) 历史上，JavaScript 的 `Date` 对象是由 Java 中的 `java.util.Date` 类启发而来的。如今，TypeScript 继承了这个设计，并给出了清晰的类型定义，使其更易于理解和使用。

(2) 如果你想要以特定的格式或是在特定的时区中显示日期，可能需要使用一些第三方库，比如 Moment.js 或是 date-fns。

(3) `Date.now()` 函数返回的是从 1970年1月1日00:00:00 UTC 开始到现在的毫秒数。而 `new Date()` 会返回一个表示当前日期和时间的 `Date` 标准对象。

## 查阅资料
1. [MDN - Date](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [TypeScript - Date 类型](https://www.typescriptlang.org/docs/handbook/basic-types.html#date)
3. [Moment.js 库](https://momentjs.com/)
4. [date-fns 库](https://date-fns.org/)