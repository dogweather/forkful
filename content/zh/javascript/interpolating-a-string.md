---
title:                "字符串插值"
date:                  2024-01-20T17:51:05.859557-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (什么和为什么?)
字符串插值是把变量的值嵌入到字符串中的过程。程序员这样做是为了让代码更灵活，可读性更强，让字符串的创建更加动态和自定义。

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
