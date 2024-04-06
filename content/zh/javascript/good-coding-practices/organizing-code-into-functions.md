---
date: 2024-01-26 01:11:13.546249-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5386\u53F2\u4E0A\uFF0C\u50CF\u65E9\u671F\
  BASIC\u6216\u6C47\u7F16\u8BED\u8A00\u8FD9\u6837\u7684\u547D\u4EE4\u5F0F\u7F16\u7A0B\
  \u8BED\u8A00\u7F3A\u4E4F\u51FD\u6570\u63D0\u4F9B\u7684\u62BD\u8C61\u80FD\u529B\u3002\
  \u968F\u7740\u65F6\u95F4\u7684\u63A8\u79FB\uFF0C\u50CFC\u8BED\u8A00\u8FD9\u6837\u7684\
  \u7F16\u7A0B\u8BED\u8A00\u5F15\u5165\u4E86\u6A21\u5757\u5316\u4EE3\u7801\u7684\u6982\
  \u5FF5\uFF0C\u610F\u5473\u7740\u5C06\u4EE3\u7801\u5206\u89E3\u6210\u5355\u4F4D\uFF08\
  \u51FD\u6570\u6216\u7A0B\u5E8F\uFF09\u4F1A\u5BFC\u81F4\u66F4\u597D\u7684\u7EC4\u7EC7\
  \u6027\u548C\u66F4\u6E05\u6670\u7684\u903B\u8F91\u3002\u2026"
lastmod: '2024-04-05T21:53:48.498253-06:00'
model: gpt-4-1106-preview
summary: "\u5728JavaScript\u4E2D\uFF0C\u9664\u4E86\u666E\u901A\u51FD\u6570\uFF0C\u81EA\
  ES6\uFF082015\u5E74\uFF09\u8D77\u6211\u4EEC\u8FD8\u5177\u6709\u7BAD\u5934\u51FD\u6570\
  \uFF0C\u5B83\u4EEC\u63D0\u4F9B\u4E86\u66F4\u7B80\u6D01\u7684\u8BED\u6CD5\uFF0C\u9002\
  \u5408\u975E\u65B9\u6CD5\u51FD\u6570."
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
weight: 18
---

## 如何操作：
```javascript
// 定义一个计算矩形面积的函数
function calculateArea(width, height) {
  return width * height;
}

// 调用函数并打印结果
let area = calculateArea(5, 3);
console.log(area); // 输出：15
```

```javascript
// 使用函数来组织相关功能
function greet(name) {
  console.log(`Hello, ${name}!`);
}

function farewell(name) {
  console.log(`Goodbye, ${name}!`);
}

greet('Alice'); // 输出：Hello, Alice!
farewell('Bob'); // 输出：Goodbye, Bob!
```

## 深入探讨
历史上，像早期BASIC或汇编语言这样的命令式编程语言缺乏函数提供的抽象能力。随着时间的推移，像C语言这样的编程语言引入了模块化代码的概念，意味着将代码分解成单位（函数或程序）会导致更好的组织性和更清晰的逻辑。

在JavaScript中，除了普通函数，自ES6（2015年）起我们还具有箭头函数，它们提供了更简洁的语法，适合非方法函数。

在JavaScript中组织代码的替代方法和增强包括使用类的面向对象方法，或视函数为一等公民的函数式编程范式。

就实现而言，JavaScript函数支持闭包，提供了一种在执行后保持对函数作用域访问的方法，这对于封装和创建工厂函数等模式来说是非常强大的。

## 另请参阅
- MDN Web 文档中的函数：https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Guide/Functions
- JavaScript 设计模式：https://addyosmani.com/resources/essentialjsdesignpatterns/book/
- JavaScript 清晰代码指南：https://github.com/ryanmcdermott/clean-code-javascript
