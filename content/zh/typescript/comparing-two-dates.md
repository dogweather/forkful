---
title:    "TypeScript: 比较两个日期"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

# 为什么要比较两个日期？

日期是编程中经常遇到的一个概念，而比较两个日期也是一个很常见的需求。比如说，我们可能需要检查某个事件是否已经过去或者需要按照日期排序。在 TypeScript 中，比较两个日期也并不复杂，下面就让我们一起来看看吧！

## 如何比较两个日期

比较两个日期最简单的方法就是使用 JavaScript 内置的 `Date` 对象。在 TypeScript 中，我们可以使用 `getDate()`、`getFullYear()`、`getMonth()` 等方法来获取日期的具体信息。下面的示例代码会演示如何比较两个日期的年份是否相同：

```typescript
// 创建两个日期对象
const date1 = new Date("2021-01-01");
const date2 = new Date("2021-06-01");

// 获取两个日期的年份
const year1 = date1.getFullYear();
const year2 = date2.getFullYear();

// 比较年份是否相同
if (year1 === year2) {
  console.log("这两个日期的年份相同");
} else {
  console.log("这两个日期的年份不同");
}
```

输出结果为：这两个日期的年份相同。

需要注意的是，`Date` 对象中的月份是从 0 开始计数的，所以 `getMonth()` 方法返回的值范围是 0-11。

除了比较年份，我们也可以比较两个日期的大小。在 TypeScript 中，我们可以通过 `getTime()` 方法来获取日期的时间戳，然后通过比较时间戳的大小来判断哪个日期更早或更晚。下面的示例演示了如何比较两个日期的大小：

```typescript
// 创建两个日期对象
const date1 = new Date("2021-01-01");
const date2 = new Date("2021-06-01");

// 获取两个日期的时间戳
const timestamp1 = date1.getTime();
const timestamp2 = date2.getTime();

// 比较时间戳的大小
if (timestamp1 < timestamp2) {
  console.log("date1 在 date2 之前");
} else if (timestamp1 === timestamp2) {
  console.log("date1 和 date2 相同");
} else {
  console.log("date1 在 date2 之后");
}
```

输出结果为：date1 在 date2 之前。

## 深入比较两个日期

在比较两个日期时，需要注意的一点是每个月的天数是不同的。比如说，2 月份可能有 28 天或 29 天，而其他月份可能有 30 天或 31 天。如果我们只是简单地比较两个日期的年份和月份，可能会忽略这种差异。所以，在深入比较两个日期时，我们需要先考虑每个月的天数，然后再进行比较。下面的示例演示了如何比较两个日期的天数：

```typescript
// 创建两个日期对象
const date1 = new Date("2021-02-13");
const date2 = new Date("2021-02-20");

// 获取两个日期的月份和日期
const month1 = date1.getMonth();
const day1 = date1.getDate();
const month2 = date2.getMonth();
const day2 = date2.getDate();

// 比较两个日期的月份和日期
if (month1 < month2) {
  console.log("date1 在 date2 之前");
} else if (month1 === month2) {
  if (day1 < day2) {
    console.log("date1 在 date2 之前");
  } else if (day1 === day2) {
    console.log("date1 和 date2 相同");
  } else {
    console.log("date1 在 date2 之后");
  }
} else {
  console.log("date1 在 date2 之后");
}
```

输出结果为：date1 在 date2