---
title:    "TypeScript: 将日期转换为字符串"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 为什么

将日期转换为字符串在程序开发中是非常常见的需求。它可以帮助开发者更方便地展示日期信息，比如在日程表或者日志中。此外，日期字符串也可以用来作为文件名或者数据库查询的参数。因此，掌握如何将日期转换为字符串对于任何喜爱编程的人来说都是必备的知识。

## 如何实现

在 TypeScript 中，我们可以使用 `toString()` 方法来将日期对象转换为字符串。它的使用方法非常简单，只需要将日期对象放在这个方法的括号里即可：

```TypeScript
let date = new Date();
let dateString = date.toString();
console.log(dateString); // 输出当前时间的日期字符串
```

除了 `toString()` 方法，我们还可以使用 `toLocaleString()` 方法来获取本地化的日期字符串。这个方法会根据当前用户的系统语言和地区来格式化日期字符串。例如，在中国的用户，使用 `toLocaleString()` 方法输出的日期字符串就会使用中文格式。

```TypeScript
let date = new Date();
let dateString = date.toLocaleString();
console.log(dateString); // 输出当前时间的本地化日期字符串
```

## 深入了解

在 TypeScript 中，日期对象其实是继承自 JavaScript 中的 `Date` 对象。`Date` 对象是基于世界标准时间（UTC）来表示时间的，但是可以根据用户所在的时区来显示不同的日期。因此，在将日期对象转换为字符串的时候，我们也需要注意时区的问题。如果我们想要输出的日期字符串使用 UTC 时间，可以使用 `toUTCString()` 方法来获取。

```TypeScript
let date = new Date();
let utcString = date.toUTCString();
console.log(utcString); // 输出当前时间的 UTC 日期字符串
```

另外，我们也可以使用 `toLocaleDateString()` 和 `toLocaleTimeString()` 方法分别获取日期和时间的字符串格式。它们还可以接收不同的参数，来指定日期和时间的显示格式。有关更多格式化日期的知识，可以查看官方文档了解更多信息。

## 参考链接

- [官方文档 - Date](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript 中文文档](https://www.tslang.cn/docs/home.html)
- [如何将日期对象转换为字符串](https://www.runoob.com/jsref/jsref-tostring-date.html)

## 参见

[如何在 TypeScript 中处理日期对象](https://www.example.com/convert-date-in-typescript)