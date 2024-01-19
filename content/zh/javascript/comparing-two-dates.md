---
title:                "比较两个日期"
html_title:           "Clojure: 比较两个日期"
simple_title:         "比较两个日期"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 什么是比较两个日期？为什么要做这种比较？

两个日期的比较就是确定哪个日期发生在前，哪个日期发生在后。作为程序员，我们需要做这种比较来处理数据，并为用户提供有用的结果。

## 怎么做：

这是一个简单的例子来演示如何在JavaScript中比较两个日期。

```Javascript
let date1 = new Date('2021-01-01');
let date2 = new Date('2021-12-31');

if(date1.getTime() < date2.getTime()) {
  console.log('date1 在 date2 之前');
} else if(date1.getTime() > date2.getTime()){
  console.log('date1 在 date2 之后');
} else {
  console.log('这两个日期是相同的');
}
```

输出结果：

```Javascript
'date1 在 date2 之前'
```

## 深入理解

一，历史背景：JavaScript自1995年诞生以来，其日期和时间的处理一直提供给我们程序员一些挑战。由于JavaScript是以网景公司的首席工程师Brendan Eich的对ECMAScript规范的实现为基础的，因此我们在JavaScript中看到的Date对象也是基于他的设计。

二，替代方案：除了JavaScript内建的Date对象，还有一些第三方的库可以处理日期，例如Moment.js和Date-fns。这些库提供了功能齐全的API，帮助我们更简单地处理和操作日期。

三，实现细节：当比较两个日期时，我们实际上在比较的是这两个日期相对于1970年1月1日以来的毫秒数。"getTime()"方法就是返回这个毫秒数。

## 更多信息

对于更多关于日期的JavaScript资源，可以查看以下链接：

1. [Mozilla 开发者网络: JavaScript Date 对象](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js: 一个强大的 JavaScript 日期库](https://momentjs.com/)
3. [Date-fns: 现代 JavaScript 日期实用工具库](https://date-fns.org/)
4. [JavaScript 标准库的 日期 和 时间 部分](http://www.ecma-international.org/ecma-262/5.1/#sec-15.9)