---
title:                "插值字符串"
html_title:           "Arduino: 插值字符串"
simple_title:         "插值字符串"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么?

字符串插值是一种插入、替换文本或表达式到字符串中的技巧。程序员使用它详细，清晰地记录他们的意图，让代码更简单，易读。

## 怎么做:

在 TypeScript 中，您可以使用反引号（`）和 ${} 来插入表达式。示例如下：

```TypeScript
let name = 'World';
console.log(`Hello, ${name}!`);  // 输出 "Hello, World!"
```

或者更复杂一些的情况：

```TypeScript
let x = 10;
let y = 20;
console.log(`The sum of ${x} and ${y} is ${x + y}.`);  // 输出 "The sum of 10 and 20 is 30."
```

## 深入探讨

1. 历史背景: 字符串插值有很长的历史，但在 JS ES6/TypeScript 中被引入，让程序员能用更自然的方式将表达式和字符串结合。

2. 可选方法: 除了字符串插值，你也可以使用 '+' 运算符或 String.concat() 方法连接字符串和变量，但这两种方法都不如字符串插值直观和灵活。

```TypeScript
let x = 10;
let y = 20;
console.log('The sum of ' + x + ' and ' + y + ' is ' + (x + y) + '.');
```

3. 实现细节: 当你使用 `${expression}` 时, TypeScript 编译器将表达式转换为字符串并插入到所在位置。

## 另请参阅

- [MDN 文档: 模板文字](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Template_literals)
- [TypeScript 官方文档: 字符串](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html#string)