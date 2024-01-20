---
title:                "从字符串解析日期"
html_title:           "C: 从字符串解析日期"
simple_title:         "从字符串解析日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么?

解析日期是将字符串转换为日期对象的过程。程序员这样做是因为它们经常需要从用户输入或数据源获取日期，并用它们进行各种操作和计算。

## 如何操作:

在Javascript中，可以使用内置的`Date`对象和它的`parse`方法来解析日期字符串。看看下面的例子:

```Javascript
let dateString = "2021-11-22";
let parsedDate = new Date(Date.parse(dateString));
console.log(parsedDate);
```

运行上面的代码，你将看到如下输出:

```Javascript
2021-11-22T00:00:00.000Z
```

## 深度探索:

- 历史背景: 早在Javascript诞生之前，日期解析就已经存在了。在那个时期，程序员需要手动分析日期字符串，这是一个繁琐且容易出错的过程。后来，Javascript和许多其他语言提供了内置的方法来处理这个问题。

- 选择方案: `Date.parse`并不是唯一的选择，你可以使用如`moment.js`等第三方库进行更复杂的日期解析和操作。

- 实现细节：`Date.parse`函数实际上将日期字符串转换为数值表示的时间戳，然后`Date`对象用这个时间戳来创建一个日期对象。

## 参考资料:

1. [Mozilla Developer Network—Date](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js—Parsing](https://momentjs.com/docs/#/parsing/)