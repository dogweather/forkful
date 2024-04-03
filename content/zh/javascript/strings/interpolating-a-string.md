---
date: 2024-01-20 17:51:05.859557-07:00
description: "How to: (\u600E\u4E48\u505A:) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:48.190176-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

## How to: (怎么做:)
```Javascript
// 基础用法
let name = "小明";
console.log(`你好, ${name}!`); // 输出: 你好, 小明!

// 表达式插值
let price = 9.99;
let taxRate = 0.07;
console.log(`总金额: ${price * (1 + taxRate)}元`); // 输出: 总金额: 10.6893元

// 函数调用
function greetings(name) {
  return `欢迎你, ${name}!`;
}
console.log(greetings("小华")); // 输出: 欢迎你, 小华!
```

## Deep Dive (深入了解)
在ES6 (ECMAScript 2015) 之前，JavaScript 中拼接字符串需要用加号 `+` 连接不同部分。ES6 引入了模板字符串(template literals)，通过反引号 ( ` ) 包围字符串，并使用 `${ }` 来插值。

替代方案包括使用加号 `+` 连接字符串或者使用 `concat` 函数。但这些方法读起来没那么直观，也不如模板字符串方便。

在内部实现上，模板字符串通过一个特殊的 `toString` 调用和字符串拼接过程转换来的。浏览器和JavaScript引擎会优化这个过程，确保它运行迅速。

## See Also (另请参阅)
- [MDN 关于模板字符串](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Template_literals)
- [ECMAScript 2015 规范](https://www.ecma-international.org/ecma-262/6.0/index.html)
