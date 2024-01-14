---
title:                "Javascript: 获取当前日期。"
simple_title:         "获取当前日期。"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 为什么获取当前日期

当我们编写一个网站或应用程序时，经常需要获取当前日期来显示在页面上或者执行特定的操作。JavaScript提供了一种简单而方便的方法来获取当前日期，让我们来看看如何做到这一点吧！

## 如何获取当前日期

首先，我们需要创建一个日期对象，通过JavaScript的内置方法 `new Date()` 来实现。这个方法将返回一个包含当前日期和时间的日期对象。我们可以将它赋值给一个变量来存储它，方便以后使用。

```Javascript
let currentDate = new Date();
```

现在，我们可以使用这个日期对象来获取我们需要的信息。例如，我们可以使用 `getFullYear()` 来获取当前年份：

```Javascript
let year = currentDate.getFullYear();
```

我们也可以使用 `getMonth()` 来获取当前月份，但是需要注意的是，这个方法返回的月份是从0开始的，所以我们需要加上1来得到实际的月份。同样的，我们可以使用 `getDate()` 来获取当前日期， `getDay()` 来获取当前星期几。

```Javascript
let month = currentDate.getMonth() + 1;
let date = currentDate.getDate();
let day = currentDate.getDay();
```

此外，我们还可以使用其他内置的方法来获取更多的日期信息，如 `getHours()` 、 `getMinutes()` 、 `getSeconds()` 等等。详细的方法可以在JavaScript的文档中找到。

## 深入了解获取当前日期

现在我们已经知道如何获取当前日期，但是你可能会好奇，JavaScript是如何知道当前日期的呢？其实，JavaScript是通过操作系统来获取当前日期的。它会调用操作系统提供的API来获取日期信息，所以它的准确性取决于当前系统的设置。

此外，JavaScript还支持使用 `new Date()` 方法传入特定的日期参数来创建一个日期对象。例如，我们可以使用日期的字符串格式作为参数，来创建对应的日期对象。

```Javascript
let specificDate = new Date("2020-10-15"); // 格式为YYYY-MM-DD
```

我们也可以使用指定的年、月、日作为参数来创建日期对象，如 `new Date(2020, 9, 15)` 表示2020年10月15日。这种方式也可以用来创建未来或过去的日期对象。

## 查看更多

希望本文能帮助你学习获取当前日期的方法。如果你想了解更多关于JavaScript日期对象的内容，请查阅以下链接：

- [MDN文档 - JavaScript日期对象](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - JavaScript日期方法](https://www.w3schools.com/js/js_dates.asp)
- [Codecademy - JavaScript的日期与时间](https://www.codecademy.com/learn/learn-javascript/modules/learn-javascript-date-and-time)

## 相关链接