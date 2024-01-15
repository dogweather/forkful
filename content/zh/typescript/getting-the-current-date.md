---
title:                "获取当前日期"
html_title:           "TypeScript: 获取当前日期"
simple_title:         "获取当前日期"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

# 为什么

首先，让我们来思考一下为什么会有人想要获取当前的日期。通常情况下，我们需要获取当前日期来进行时间相关的操作，比如在日志中记录当前时间，或者在计时功能中显示当前时间。

## 如何操作

要获取当前的日期，我们可以使用TypeScript语言中的内置函数`Date()`。以下是一个简单的示例代码：

```TypeScript
let currentDate = new Date();
console.log(currentDate);
```

运行以上代码，会得到当前日期的字符串输出，例如`Sat Jan 23 2021 11:39:22 GMT+0800`。我们也可以使用`toLocaleDateString()`函数来对日期进行格式化，比如只显示年月日：

```TypeScript
let currentDate = new Date();
let formattedDate = currentDate.toLocaleDateString();
console.log(formattedDate);
```

输出结果为`1/23/2021`。除此之外，我们还可以通过使用第三方库来更加灵活地获取和处理当前日期，比如`moment.js`。

## 深入了解

在深入了解获取当前日期的过程中，我们需要理解一些基本概念。首先，计算机内部会将时间转换成一种数字形式，称为“时间戳”。这个数字表示自公元1970年1月1日0时0分0秒以来经过的毫秒数。因此，每次我们获取当前日期，计算机实际上是将这个数字转换成我们熟悉的日期格式。

另外，我们也需要了解一些关于时区的知识。由于地球的自转速度不均匀，且不同地区的太阳当地时间也不同，于是人类为了方便统一起见，将地球分为不同的时区，每个时区都有自己的标准时间。当我们获取当前日期时，根据我们所处的时区不同，输出的结果也会有所差异。

## 参考链接

- [TypeScript官方文档：Date对象](https://www.typescriptlang.org/docs/handbook/2/writing-declarations.html#classes)
- [Moment.js官方文档](https://momentjs.com/docs/)
- [维基百科：时间戳](https://zh.wikipedia.org/wiki/%E6%97%B6%E9%97%B4%E6%88%B3)
- [维基百科：时区](https://zh.wikipedia.org/wiki/%E6%97%B6%E5%8C%BA)

## 参见