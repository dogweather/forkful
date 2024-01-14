---
title:    "Javascript: 在计算机编程中，这是一篇关于“计算未来或过去日期”的文章。"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

如果你是一位喜欢编程的朋友，那么你可能经常会遇到需要计算未来或过去日期的问题。这其中涉及到许多项目，比如计划旅行日期、制定任务截止日期等。在这篇文章中，我们将深入了解如何使用Javascript编程来轻松计算未来或过去的日期。

## Why
为什么要计算未来或过去的日期？对于需要制定时间计划和任务安排的人来说，计算日期是一项非常重要的技能。例如，如果你计划出国旅行，你需要知道何时预订机票和酒店，何时安排签证申请等。通过计算日期，你可以更有效地管理自己的时间，并确保计划顺利进行。因此，学习如何用Javascript编程来计算日期将是非常有用的。

## How To
```javascript
// 计算未来日期的函数
function calculateFutureDate(days) {
  let today = new Date();
  let futureDate = new Date(today.getTime() + days * 24 * 60 * 60 * 1000); // 将天数转换为毫秒数，再加到现在的日期中
  return futureDate;
}

let futureDate = calculateFutureDate(10); // 计算10天后的日期
console.log(`10天后的日期是: ${futureDate}`);

// 计算过去日期的函数
function calculatePastDate(days) {
  let today = new Date();
  let pastDate = new Date(today.getTime() - days * 24 * 60 * 60 * 1000); // 将天数转换为毫秒数，再从现在的日期中减去
  return pastDate;
}

let pastDate = calculatePastDate(5); // 计算5天前的日期
console.log(`5天前的日期是: ${pastDate}`);

// 输出:
// 10天后的日期是: Tue Apr 20 2021 09:53:46 GMT+0800 (China Standard Time)
// 5天前的日期是: Thu Apr 15 2021 09:53:46 GMT+0800 (China Standard Time)
```

## Deep Dive
在Javascript中，我们可以使用Date对象来表示日期和时间。它有一些内置的方法，比如getTime()来获取当前日期的毫秒数，getDay()来获取当前日期是星期几等。我们可以利用这些方法来计算日期。

在计算未来日期时，我们首先需要获取当前的日期，然后通过将未来的天数转换为毫秒数，再加到当前日期中来得到未来的日期。同样，在计算过去日期时，我们需要获取当前的日期，然后通过将过去的天数转换为毫秒数，再从当前日期中减去来得到过去的日期。

除了利用内置的方法来计算日期，我们还可以使用第三方库，比如Moment.js来更方便地处理日期和时间。它提供了许多实用的方法，可以让我们更轻松地计算日期和时间。

## See Also
- [MDN: Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)