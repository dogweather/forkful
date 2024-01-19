---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

解析日期字符串，就是把写有日期的字符串转换成计算机能理解的日期格式。这是程序员常常使用的操作，你可以用它来获取和处理用户的输入，或者解析各种日期格式。

## 如何操作:

以下是几个关于如何在 TypeScript 中解析日期字符串的实例

```TypeScript
let myDate = new Date('2021-09-01');

console.log(myDate);
// 输出: 2021-08-31T16:00:00.000Z
```
在这个例子中，我们创建了一个新的 `Date` 对象并把一个日期字符串 `'2021-09-01'` 作为输入。输出的日期取决于你所在的时区。

另一个例子会把日期和时间一起解析:

```TypeScript
let myDateTime = new Date('2021-09-01T22:00:00');

console.log(myDateTime);
// 输出: 2021-09-01T16:00:00.000Z
```
这个例子输入的字符串格式是 `'2021-09-01T22:00:00'`。字符串里面包含了日期和时间。

## 深入研究

历史上，JavaScript 的 `Date` 对象是用来解析日期的。但是，由于 JavaScript 的设计缺陷，`Date` 对象在处理日期时有时会出现不确定性。这就是为什么在 ECMAScript 5（TypeScript 的一个版本）中,开始引入更为严格的日期格式，如 'YYYY-MM-DDTHH:mm:ss.sssZ'。

而在 TypeScript 中，一个替代品是使用 `moment.js` 库去解析日期。这个库允许更灵活的日期解析和格式化，但是需要额外导入和使用。

解析日期字符串的具体实现其实是有一些难度的。尤其是需要处理不同的日期格式和时区问题。幸运的是，浏览器为我们提供了大部分的工具和函数，让我们的工作变得更简单。

## 另请参见

如果你想了解更多关于 `Date` 对象和日期解析的内容，可以查看以下链接:

- [MDN Web Docs: Date](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Moment.js](https://momentjs.com/)
- [ECMAScript 5 specification](https://www.ecma-international.org/publications/standards/Ecma-262.htm)