---
date: 2024-01-20 17:37:01.015102-07:00
description: "\u4EC0\u4E48\u4EE5\u53CA\u4E3A\u4EC0\u4E48\uFF1F JavaScript\u4E2D\uFF0C\
  \u5C06\u65E5\u671F\u8F6C\u6362\u6210\u5B57\u7B26\u4E32\u662F\u628ADate\u5BF9\u8C61\
  \u8868\u793A\u4E3A\u6587\u672C\u683C\u5F0F\u3002\u8FD9\u6837\u505A\u53EF\u4EE5\u8BA9\
  \u65E5\u671F\u66F4\u5BB9\u6613\u9605\u8BFB\u548C\u5B58\u50A8\uFF0C\u5C24\u5176\u662F\
  \u5728\u9700\u8981\u4EE5\u7279\u5B9A\u683C\u5F0F\u5C55\u793A\u6216\u5728\u7F51\u7EDC\
  \u4E0A\u4F20\u8F93\u65F6\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.228240-06:00'
model: gpt-4-1106-preview
summary: "\u4EC0\u4E48\u4EE5\u53CA\u4E3A\u4EC0\u4E48\uFF1F\n\nJavaScript\u4E2D\uFF0C\
  \u5C06\u65E5\u671F\u8F6C\u6362\u6210\u5B57\u7B26\u4E32\u662F\u628ADate\u5BF9\u8C61\
  \u8868\u793A\u4E3A\u6587\u672C\u683C\u5F0F\u3002\u8FD9\u6837\u505A\u53EF\u4EE5\u8BA9\
  \u65E5\u671F\u66F4\u5BB9\u6613\u9605\u8BFB\u548C\u5B58\u50A8\uFF0C\u5C24\u5176\u662F\
  \u5728\u9700\u8981\u4EE5\u7279\u5B9A\u683C\u5F0F\u5C55\u793A\u6216\u5728\u7F51\u7EDC\
  \u4E0A\u4F20\u8F93\u65F6\u3002."
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
