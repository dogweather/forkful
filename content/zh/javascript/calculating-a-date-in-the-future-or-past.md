---
title:    "Javascript: 计算未来或过去的日期"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# 为什么要计算未来或过去的日期？

计算未来或过去的日期对于编程具有很大的实用性，可以帮助我们创建具有时间限制的应用程序或提醒功能。例如，在日历应用程序中，我们可以使用编程来计算未来某一天的日期并提醒用户。因此，学习如何计算日期是非常必要的。

## 如何做？

```Javascript
// 计算未来的日期，比如在当前日期上加上7天，并输出格式为"月/日/年"
let date = new Date();
let futureDate = new Date(date.getTime() + 7 * 24 * 60 * 60 * 1000); // 获取当前日期并加上7天的毫秒数
// 使用内置函数获取月、日、年，并用字符串拼接的方式输出
console.log(`${futureDate.getMonth() + 1}/${futureDate.getDate()}/${futureDate.getFullYear()}`);
// 输出结果： 6/17/2021
```

```Javascript
// 计算过去的日期，比如在当前日期上减去30天，并输出格式为"月/日/年"
let date = new Date();
let pastDate = new Date(date.getTime() - 30 * 24 * 60 * 60 * 1000); // 获取当前日期并减去30天的毫秒数
// 使用内置函数获取月、日、年，并用字符串拼接的方式输出
console.log(`${pastDate.getMonth() + 1}/${pastDate.getDate()}/${pastDate.getFullYear()}`);
// 输出结果： 5/18/2021
```

## 深入探讨

在Javascript中，我们可以使用Date对象和其内置方法来计算日期。在计算日期之前，我们需要先获取当前日期，这可以通过使用new Date()来实现。然后，我们可以使用Date对象的方法来获取月、日、年等信息，并在计算未来或过去的日期时，根据需要加上相应的毫秒数。最后，我们可以使用字符串拼接的方式来输出日期的格式。

此外，我们还可以使用第三方库如moment.js来方便地计算日期。

# 参考链接

- [阮一峰的《Javascript Date 对象》](https://wangdoc.com/javascript/types/date.html)
- [MDN Web Docs中文版的Date对象文档](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [moment.js官方文档](https://momentjs.com/docs/)