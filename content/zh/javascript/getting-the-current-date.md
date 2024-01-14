---
title:    "Javascript: 获取当前日期。"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

# 为什么要获取当前日期？

在Javascript编程中，经常会遇到需要获取当前日期的情况。无论是制作一个日历应用程序，还是在电商网站上显示限时优惠，获取当前日期都是必不可少的。通过获取当前日期，我们可以让程序更加智能化、灵活性更强。

# 如何获取当前日期？

在Javascript中，我们可以通过内置的`Date()`对象来获取当前日期。使用`new Date()`构造函数，我们可以创建一个表示当前日期和时间的对象实例。然后，我们可以使用`getFullYear()`、`getMonth()`、`getDate()`等方法来获取具体的年份、月份、日期等信息。让我们来看一个例子：

```Javascript
// 创建一个Date对象实例
let currentDate = new Date();

// 使用 getFullYear() 方法来获取当前年份
let currentYear = currentDate.getFullYear();
console.log(currentYear); // 输出：2021

// 使用 getMonth() 方法来获取当前月份
let currentMonth = currentDate.getMonth() + 1; // 返回的月份是从0-11表示的，所以需要加1
console.log(currentMonth); // 输出：7

// 使用 getDate() 方法来获取当前日期
let currentDay = currentDate.getDate();
console.log(currentDay); // 输出：14
```

除了以上几个常用方法外，`Date()`对象还有许多其他有用的方法，例如`getDay()`可以获取当前日期是星期几，`getHours()`可以获取当前小时数，`getMinutes()`可以获取当前分钟数等等。具体可以查看[MDN文档](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)进行深入学习。

# 深入了解当前日期获取

在Javascript中，日期和时间是以计算机内部的一个数值来表示的。具体来说，它是距离1970年1月1日 00:00:00 UTC的毫秒数。这个数值可以是正数也可以是负数，表示的是当前日期和时间与1970年1月1日之间的时间间隔。通过这个数值，我们可以进行日期的计算、比较，甚至可以设置一个日期倒计时。这对于制作网页应用程序来说是非常有用的。

另外，`Date()`对象还支持一些有用的格式化方法，例如`toLocalDateString()`可以将当前日期以本地的形式显示。如果要实现更复杂的功能，也可以使用第三方的日期库，例如Moment.js。

# 参考资料

- [MDN文档：获取当前日期](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3School：Javascript日期对象](https://www.w3school.com.cn/js/js_dates.asp)
- [Moment.js官网](https://momentjs.com/)
- [时间日期格式化的一些方法](https://segmentfault.com/a/1190000014457789) 

# 另请参阅

- [Javascript中内置的Date对象](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [日期操作库Moment.js的使用教程](https://github.com/moment/moment)
- [使用Javascript计算日期倒计时](https://www.w3schools.com/howto/howto_js_countdown.asp)