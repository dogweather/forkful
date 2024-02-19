---
aliases:
- /zh/javascript/working-with-complex-numbers/
date: 2024-01-26 04:42:32.926158-07:00
description: "\u590D\u6570\u662F\u5177\u6709\u5B9E\u90E8\u548C\u865A\u90E8\u7684\u6570\
  \uFF08\u5982 3 + 4i\uFF09\u3002\u5B83\u4EEC\u5728\u5404\u79CD\u7F16\u7A0B\u95EE\u9898\
  \u4E2D\u51FA\u73B0\uFF0C\u7279\u522B\u662F\u5728\u4FE1\u53F7\u5904\u7406\u3001\u91CF\
  \u5B50\u8BA1\u7B97\u548C\u89E3\u591A\u9879\u5F0F\u65B9\u7A0B\u4E2D\u3002\u7A0B\u5E8F\
  \u5458\u901A\u8FC7\u64CD\u4F5C\u5B83\u4EEC\u6765\u6709\u6548\u5730\u89E3\u51B3\u8FD9\
  \u7C7B\u4EFB\u52A1\u3002"
lastmod: 2024-02-18 23:08:59.473967
model: gpt-4-0125-preview
summary: "\u590D\u6570\u662F\u5177\u6709\u5B9E\u90E8\u548C\u865A\u90E8\u7684\u6570\
  \uFF08\u5982 3 + 4i\uFF09\u3002\u5B83\u4EEC\u5728\u5404\u79CD\u7F16\u7A0B\u95EE\u9898\
  \u4E2D\u51FA\u73B0\uFF0C\u7279\u522B\u662F\u5728\u4FE1\u53F7\u5904\u7406\u3001\u91CF\
  \u5B50\u8BA1\u7B97\u548C\u89E3\u591A\u9879\u5F0F\u65B9\u7A0B\u4E2D\u3002\u7A0B\u5E8F\
  \u5458\u901A\u8FC7\u64CD\u4F5C\u5B83\u4EEC\u6765\u6709\u6548\u5730\u89E3\u51B3\u8FD9\
  \u7C7B\u4EFB\u52A1\u3002"
title: "\u5904\u7406\u590D\u6570"
---

{{< edit_this_page >}}

## 是什么 & 为什么？
复数是具有实部和虚部的数（如 3 + 4i）。它们在各种编程问题中出现，特别是在信号处理、量子计算和解多项式方程中。程序员通过操作它们来有效地解决这类任务。

## 如何操作：
JavaScript 没有内置的复数支持，但你可以卷起袖子，使用对象和数学来处理。这里有一个快速的方法。

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...根据需要添加更多方法（减法，乘法，除法）

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const result = a.add(b);

console.log(`结果: ${result}`); // 结果: 4 + 6i
```

## 深入探讨
自16世纪以来，复数一直存在，归功于意大利数学家 Gerolamo Cardano。它们在各个领域变得至关重要，比如工程和物理学。在现代编程中，对于需要多维度的模拟和算法，它们是关键。

现在，JavaScript 并没有原生支持复数。但除了DIY选项外，你还可以使用数学库，如 math.js 或 numeric.js。它们为更重的复数处理提供了动力，添加了更多操作，幅度计算和参数查找等优点。

在底层，当你使用复数进行操作时，就像管理两个紧密相连的单独数字一样。加法和减法是直接进行的——实部对应实部，虚部对应虚部。乘法和除法在处理交叉项时会变得复杂，需要更多的关注。

## 另见
- JavaScript 的 MDN Web Docs：https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js，一个包含复数的数学库：https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js，另一个库：http://numericjs.com/documentation.html
- 关于复数的更深入探讨（专注于数学）：https://mathworld.wolfram.com/ComplexNumber.html
