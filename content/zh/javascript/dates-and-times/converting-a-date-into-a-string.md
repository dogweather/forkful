---
date: 2024-01-20 17:37:01.015102-07:00
description: "How to: \u6DF1\u5165\u4E86\u89E3\uFF1A \u5C06\u65E5\u671F\u8F6C\u6362\
  \u6210\u5B57\u7B26\u4E32\u6709\u5404\u79CD\u65B9\u6CD5\uFF0C\u6BCF\u79CD\u65B9\u6CD5\
  \u6839\u636E\u4E0D\u540C\u7684\u76EE\u7684\u548C\u683C\u5F0F\u9700\u6C42\u800C\u4EA7\
  \u751F\u3002\u6700\u521D\u7684`toString`\u65B9\u6CD5\u80FD\u591F\u7ED9\u51FA\u5B8C\
  \u6574\u7684\u65E5\u671F\u548C\u65F6\u95F4\uFF0C\u4F46\u901A\u5E38\u5305\u542B\u65F6\
  \u533A\u4FE1\u606F\uFF0C\u5BF9\u4E8E\u9700\u8981\u7279\u5B9A\u65E5\u671F\u683C\u5F0F\
  \u7684\u573A\u666F\u5E76\u4E0D\u53CB\u597D\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.504469-06:00'
model: gpt-4-1106-preview
summary: "\u6DF1\u5165\u4E86\u89E3\uFF1A \u5C06\u65E5\u671F\u8F6C\u6362\u6210\u5B57\
  \u7B26\u4E32\u6709\u5404\u79CD\u65B9\u6CD5\uFF0C\u6BCF\u79CD\u65B9\u6CD5\u6839\u636E\
  \u4E0D\u540C\u7684\u76EE\u7684\u548C\u683C\u5F0F\u9700\u6C42\u800C\u4EA7\u751F\u3002\
  \u6700\u521D\u7684`toString`\u65B9\u6CD5\u80FD\u591F\u7ED9\u51FA\u5B8C\u6574\u7684\
  \u65E5\u671F\u548C\u65F6\u95F4\uFF0C\u4F46\u901A\u5E38\u5305\u542B\u65F6\u533A\u4FE1\
  \u606F\uFF0C\u5BF9\u4E8E\u9700\u8981\u7279\u5B9A\u65E5\u671F\u683C\u5F0F\u7684\u573A\
  \u666F\u5E76\u4E0D\u53CB\u597D."
title: "\u5C06\u65E5\u671F\u8F6C\u6362\u4E3A\u5B57\u7B26\u4E32"
weight: 28
---

## How to:
怎么做：

```javascript
// 创建一个新的Date对象
let now = new Date();

// 转换为字符串 - 默认toString方法
console.log(now.toString()); // 输出："Wed Mar 25 2023 15:45:30 GMT+0800 (China Standard Time)"

// 使用toDateString()获取日期部分
console.log(now.toDateString()); // 输出："Wed Mar 25 2023"

// 使用toTimeString()获取时间部分
console.log(now.toTimeString()); // 输出："15:45:30 GMT+0800 (China Standard Time)"

// 使用toISOString()针对ISO格式
console.log(now.toISOString()); // 输出："2023-03-25T07:45:30.000Z"

// 使用toLocaleString()针对地区格式
console.log(now.toLocaleString('zh-CN')); // 输出："2023/3/25 下午3:45:30"
```

## Deep Dive
深入了解：

将日期转换成字符串有各种方法，每种方法根据不同的目的和格式需求而产生。最初的`toString`方法能够给出完整的日期和时间，但通常包含时区信息，对于需要特定日期格式的场景并不友好。

`toDateString()`和`toTimeString()`是历史较长的方法，提供了日期和时间的分离。`toISOString()`是一个较新的标准，提供了一个遵循ISO 8601标准的日期时间格式，特别适合在网络上交换数据。而`toLocaleString()`则允许根据不同的地区标准来展示日期和时间。

除了内置方法，还可以用其他库，如`moment.js`或`date-fns`更灵活地格式化日期。这些库可以简化代码，提供更多语言选项和格式。

## See Also
另请参阅：

- MDN Web Docs上的Date对象：[Date - JavaScript | MDN](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- `moment.js`库：[Moment.js | Home](https://momentjs.com/)
- `date-fns`库：[date-fns - modern JavaScript date utility library](https://date-fns.org/)
