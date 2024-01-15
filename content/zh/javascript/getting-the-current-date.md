---
title:                "获取当前日期"
html_title:           "Javascript: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么
有时候，我们需要获取当前日期来做一些事情，比如显示在一个网页上，记录事件，或者进行其他相关操作。JavaScript提供了一种简单的方法来获取当前日期，让我们来深入了解一下吧！

## 如何获取当前日期
获取当前日期的方法是使用 JavaScript 中内置的`Date()`对象。这个对象包含了有关日期和时间的方法，让我们来看看如何使用它来获取当前日期。

```javascript
let currentDate = new Date(); // 创建一个 Date 对象
console.log(currentDate); // 输出当前日期和时间
```

这段代码中，我们创建了一个名为`currentDate`的变量，并使用`new Date()`来创建一个`Date`对象并将其赋值给变量。接着，我们使用`console.log()`方法来输出这个对象，它会在控制台中显示当前的日期和时间。

除了使用`console.log()`来输出日期对象，我们也可以使用日期对象中内置的方法来获取特定的信息，比如年份、月份、日期和时间等。让我们看一个例子：

```javascript
let currentDate = new Date();
let currentYear = currentDate.getFullYear(); // 使用 getFullYear() 方法来获取当前年份
console.log(currentYear); // 输出当前年份
```

上面的代码中，我们首先创建了一个`currentDate`变量并获取了当前日期对象，然后通过`getFullYear()`方法来获取当前年份并赋值给`currentYear`变量。最后，我们使用`console.log()`来输出这个变量，从而输出当前年份。

## 深入了解
除了上面介绍的`Date()`对象和它的一些方法外，JavaScript还提供了一些其他方法来获取当前日期和时间。

- `getMonth()`：获取指定的月份，返回值为一个数字，范围为0-11，其中0代表一月，1代表二月，以此类推。
- `getDate()`：获取指定日期，返回值为一个数字，范围为1-31。
- `getHours()`：获取指定小时，返回值为一个数字，范围为0-23。
- `getMinutes()`：获取指定分钟，返回值为一个数字，范围为0-59。
- `getSeconds()`：获取指定秒数，返回值为一个数字，范围为0-59。

可以使用类似上面示例的方法来获取上述信息，并将它们赋值给相应的变量。

## 参考链接
- [MDN web docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript Dates](https://www.w3schools.com/js/js_dates.asp)
- [Getting the Date, Month, and Year in JavaScript](https://stackoverflow.com/questions/1531093/how-do-i-get-the-current-date-in-javascript)

## 请参阅
- [如何使用JavaScript设置计时器](https://github.com/yixiuer/Article-Templates/blob/main/Timer_Setup.md)