---
date: 2024-01-26 04:46:30.687808-07:00
description: "\u590D\u6570\u7531\u5B9E\u90E8\u548C\u865A\u90E8\u7EC4\u6210\uFF08\u901A\
  \u5E38\u5199\u4F5C a + bi\uFF09\uFF0C\u5B83\u4EEC\u4F7F\u5F97\u67D0\u4E9B\u53EA\u7528\
  \u5B9E\u6570\u65E0\u6CD5\u5B9E\u73B0\u6216\u5B9E\u8DF5\u56F0\u96BE\u7684\u8BA1\u7B97\
  \u6210\u4E3A\u53EF\u80FD\u3002\u7A0B\u5E8F\u5458\u5728\u8BF8\u5982\u4FE1\u53F7\u5904\
  \u7406\u3001\u91CF\u5B50\u8BA1\u7B97\u548C\u5E94\u7528\u6570\u5B66\u7B49\u9886\u57DF\
  \u4F7F\u7528\u590D\u6570\uFF0C\u8FD9\u4E9B\u9886\u57DF\u4E2D\u4E8C\u7EF4\u6570\u8868\
  \u793A\u81F3\u5173\u91CD\u8981\u3002"
lastmod: '2024-02-25T18:49:45.028358-07:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\u7531\u5B9E\u90E8\u548C\u865A\u90E8\u7EC4\u6210\uFF08\u901A\
  \u5E38\u5199\u4F5C a + bi\uFF09\uFF0C\u5B83\u4EEC\u4F7F\u5F97\u67D0\u4E9B\u53EA\u7528\
  \u5B9E\u6570\u65E0\u6CD5\u5B9E\u73B0\u6216\u5B9E\u8DF5\u56F0\u96BE\u7684\u8BA1\u7B97\
  \u6210\u4E3A\u53EF\u80FD\u3002\u7A0B\u5E8F\u5458\u5728\u8BF8\u5982\u4FE1\u53F7\u5904\
  \u7406\u3001\u91CF\u5B50\u8BA1\u7B97\u548C\u5E94\u7528\u6570\u5B66\u7B49\u9886\u57DF\
  \u4F7F\u7528\u590D\u6570\uFF0C\u8FD9\u4E9B\u9886\u57DF\u4E2D\u4E8C\u7EF4\u6570\u8868\
  \u793A\u81F3\u5173\u91CD\u8981\u3002"
title: "\u5904\u7406\u590D\u6570"
---

{{< edit_this_page >}}

## 什么 & 为什么？
复数由实部和虚部组成（通常写作 a + bi），它们使得某些只用实数无法实现或实践困难的计算成为可能。程序员在诸如信号处理、量子计算和应用数学等领域使用复数，这些领域中二维数表示至关重要。

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
