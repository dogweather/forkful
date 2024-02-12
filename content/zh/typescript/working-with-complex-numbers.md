---
title:                "处理复数"
aliases:
- zh/typescript/working-with-complex-numbers.md
date:                  2024-01-26T04:46:30.687808-07:00
model:                 gpt-4-0125-preview
simple_title:         "处理复数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/working-with-complex-numbers.md"
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
