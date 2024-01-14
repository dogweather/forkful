---
title:    "TypeScript: 把日期转换成字符串"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## 为什么要将日期转换为字符串

在编写程序时，将日期转换为字符串是非常常见的需求。比如，当我们需要在用户界面上展示日期时，通常需要将日期转换为字符串。此外，将日期转换为字符串也有利于日期的格式化，使其更容易阅读和理解。

## 如何将日期转换为字符串

```TypeScript
// 创建一个日期对象
let date = new Date();

// 使用 Date 类的 toUTCString() 方法将日期转换为字符串
let stringDate = date.toUTCString();

// 输出结果为 "Thu, 10 Jun 2021 12:00:00 GMT"
console.log(stringDate);

// 使用 Date 类的 toDateString() 方法将日期转换为字符串，只保留日期部分
let dateString = date.toDateString();

// 输出结果为 "Thu Jun 10 2021"
console.log(dateString);
```

## 深入探讨日期转换为字符串

在 TypeScript 中，日期对象和字符串之间的转换使用了 Date 类的不同方法。toUTCString() 方法将日期转换为一个国际标准的字符串，而 toDateString() 方法则将日期转换为一个缩短的日期字符串。此外，我们还可以使用 Date 类的其他方法来实现更多不同的日期和字符串转换，具体可以查看官方文档。

## 参考链接

- [TypeScript Date 类官方文档](https://www.typescriptlang.org/docs/handbook/2/objects.html#classes)
- [JavaScript Date 对象参考文档](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript 字符串类型参考文档](https://www.typescriptlang.org/docs/handbook/2/objects.html#string)