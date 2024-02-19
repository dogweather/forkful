---
aliases:
- /zh/javascript/organizing-code-into-functions/
date: 2024-01-26 01:11:13.546249-07:00
description: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u4E3A\u51FD\u6570\u53EF\u4EE5\u5C06\u4EFB\
  \u52A1\u5212\u5206\u4E3A\u53EF\u91CD\u590D\u4F7F\u7528\u7684\u7247\u6BB5\uFF0C\u4F7F\
  \u4EE3\u7801\u66F4\u52A0\u6E05\u6670\u4E14\u6613\u4E8E\u7EF4\u62A4\u3002\u901A\u8FC7\
  \u8FD9\u6837\u505A\u53EF\u4EE5\u51CF\u5C11\u5197\u4F59\u3001\u7B80\u5316\u6D4B\u8BD5\
  \u5E76\u63D0\u9AD8\u53EF\u8BFB\u6027\u3002"
lastmod: 2024-02-18 23:08:59.484419
model: gpt-4-1106-preview
summary: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u4E3A\u51FD\u6570\u53EF\u4EE5\u5C06\u4EFB\
  \u52A1\u5212\u5206\u4E3A\u53EF\u91CD\u590D\u4F7F\u7528\u7684\u7247\u6BB5\uFF0C\u4F7F\
  \u4EE3\u7801\u66F4\u52A0\u6E05\u6670\u4E14\u6613\u4E8E\u7EF4\u62A4\u3002\u901A\u8FC7\
  \u8FD9\u6837\u505A\u53EF\u4EE5\u51CF\u5C11\u5197\u4F59\u3001\u7B80\u5316\u6D4B\u8BD5\
  \u5E76\u63D0\u9AD8\u53EF\u8BFB\u6027\u3002"
title: "\u5C06\u4EE3\u7801\u7EC4\u7EC7\u6210\u51FD\u6570"
---

{{< edit_this_page >}}

## 何为之与为何？
将代码组织为函数可以将任务划分为可重复使用的片段，使代码更加清晰且易于维护。通过这样做可以减少冗余、简化测试并提高可读性。

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
