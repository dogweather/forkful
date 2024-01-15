---
title:                "将日期转换为字符串"
html_title:           "TypeScript: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

在编写Web应用程序时，经常需要将日期转换为字符串以便在用户界面上显示。使用TypeScript中的内置函数可以轻松实现这一任务，提高代码的可读性和灵活性。

## 如何

```TypeScript
let date = new Date(); // 创建当前日期对象
let dateString = date.toDateString(); // 转换为可读的日期字符串
console.log(dateString); // 输出：Mon Jul 05 2021
```

在上面的示例中，我们通过调用内置的toDateString()函数来将日期对象转换为字符串，并将其存储在变量dateString中。该函数将返回一个可读的日期字符串，如“Mon Jul 05 2021”。我们还可以使用其他内置函数如toTimeString()来获取时间字符串或者使用toLocaleDateString()来获取特定格式的日期字符串。

## 深入探讨

在JavaScript中，日期对象是以毫秒数值的形式存储的，表示自1970年1月1日午夜（UTC时间）起经过的毫秒数。这个毫秒数值与时区无关，因此在进行日期转换时需要注意时区的影响。同时，调用toDateString()、toTimeString()等函数时，会根据用户的本地时区显示日期和时间格式。

## 参考链接

- [TypeScript官方文档](https://www.typescriptlang.org/docs/handbook/)
- [JavaScript中的日期对象](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript中的日期类型](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-4-1.html#new-keywords-for-dynamic-import-expressions)