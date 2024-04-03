---
date: 2024-01-26 04:46:30.687808-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728 TypeScript \u4E2D\u5904\u7406\u590D\
  \u6570\u9700\u8981\u4E00\u4E2A\u4E13\u7528\u7684\u7C7B\u3002\u8BA9\u6211\u4EEC\u521B\
  \u5EFA\u4E00\u4E2A\uFF0C\u5E76\u901A\u8FC7\u6DFB\u52A0\u548C\u4E58\u6CD5\u64CD\u4F5C\
  \u6765\u5B9E\u8DF5\u5B83\u3002"
lastmod: '2024-03-13T22:44:47.465211-06:00'
model: gpt-4-0125-preview
summary: "\u5728 TypeScript \u4E2D\u5904\u7406\u590D\u6570\u9700\u8981\u4E00\u4E2A\
  \u4E13\u7528\u7684\u7C7B\u3002\u8BA9\u6211\u4EEC\u521B\u5EFA\u4E00\u4E2A\uFF0C\u5E76\
  \u901A\u8FC7\u6DFB\u52A0\u548C\u4E58\u6CD5\u64CD\u4F5C\u6765\u5B9E\u8DF5\u5B83."
title: "\u5904\u7406\u590D\u6570"
weight: 14
---

## 如何操作：
在 TypeScript 中处理复数需要一个专用的类。让我们创建一个，并通过添加和乘法操作来实践它。

```TypeScript
class Complex {
    constructor(public re: number, public im: number) {}

    add(other: Complex): Complex {
        return new Complex(this.re + other.re, this.im + other.im);
    }

    multiply(other: Complex): Complex {
        return new Complex(
            this.re * other.re - this.im * other.im,
            this.re * other.im + this.im * other.re
        );
    }

    toString(): string {
        return `${this.re} + ${this.im}i`;
    }
}

let num1 = new Complex(1, 2);
let num2 = new Complex(3, 4);
let sum = num1.add(num2);
let product = num1.multiply(num2);

console.log(`Sum: ${sum.toString()}`); // 输出：Sum: 4 + 6i
console.log(`Product: ${product.toString()}`); // 输出：Product: -5 + 10i
```

## 深入了解
从历史上看，复数曾是有争议的 - 甚至被称为“虚数”以表达最初的怀疑态度。现在，它们在现代数学和科学中是基础性的。

作为我们简单类的替代方法可能涉及使用现有的库，如 `math.js` 或 `complex.js`，它们具有附加功能，如三角函数、指数函数和复共轭。

我们的 TypeScript 实现细节归结为定义算术运算。`add` 方法简单地将对应的部分相加。`multiply` 应用了代数中使用的FOIL方法，记住 `i^2 = -1`。

## 参阅
欲了解更多关于复数及其在编程中的使用的阅读材料和资源，请查看：

- MDN 复数代数：https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- `math.js` 库：https://mathjs.org/docs/datatypes/complex_numbers.html
- `complex.js` 库：https://complex-js.github.io/complex.js/
