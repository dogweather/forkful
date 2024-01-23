---
title:                "从字符串解析日期"
date:                  2024-01-20T15:36:52.597187-07:00
html_title:           "Arduino: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)

解析日期字符串是把文本形式的日期转换成编程语言可以理解的日期对象。这样做可以让我们在代码中处理日期，比如计算时间差或格式化输出。

## How to: (怎么做：)

```Javascript
// 基本例子
let dateString = "2023-04-15";
let parsedDate = new Date(dateString);
console.log(parsedDate); // 输出：Sat Apr 15 2023 08:00:00 GMT+0800 (中国标准时间)

// 使用 Date.parse()
let timeStamp = Date.parse(dateString);
console.log(new Date(timeStamp)); // 输出：Sat Apr 15 2023 08:00:00 GMT+0800 (中国标准时间)

// 考虑时区
let dateStringWithTimezone = "2023-04-15T12:00:00Z";
let parsedDateWithTimezone = new Date(dateStringWithTimezone);
console.log(parsedDateWithTimezone); // 输出：Sat Apr 15 2023 20:00:00 GMT+0800 (中国标准时间)
```

## Deep Dive (深入探究)

在JavaScript早期，日期字符串的解析并不标准，导致在不同的浏览器有不同的结果。ECMAScript 5定义了日期时间字符串的标准格式（ISO 8601），从那时起，解析变得可靠多了。

除了上面的 `Date` 对象，你也可以使用第三方库比如 Moment.js 或 date-fns 来解析日期。它们提供了额外的功能和灵活性，尤其是在处理不同的日期格式和时区时。

浏览器的实现细节：大多数现代浏览器都遵循 ECMAScript 规范，但在解析非标准格式的日期时仍可能出现兼容性问题。

## See Also (另请参阅)

- MDN Web Docs on Date: [https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js: [https://momentjs.com/](https://momentjs.com/)
- date-fns: [https://date-fns.org/](https://date-fns.org/)
