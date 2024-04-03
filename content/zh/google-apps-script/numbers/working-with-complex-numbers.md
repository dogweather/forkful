---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:49.729030-07:00
description: "\u590D\u6570\uFF0C\u4F5C\u4E3A\u5B9E\u90E8\u548C\u865A\u90E8\u7684\u7EC4\
  \u5408\u8868\u73B0\u5F62\u5F0F\uFF08\u4F8B\u5982\uFF0C3 + 4i\uFF09\uFF0C\u5728\u5404\
  \u79CD\u8BA1\u7B97\u95EE\u9898\u4E2D\u90FD\u662F\u57FA\u672C\u7684\uFF0C\u7279\u522B\
  \u662F\u5728\u5DE5\u7A0B\u3001\u7269\u7406\u548C\u5E94\u7528\u6570\u5B66\u7B49\u9886\
  \u57DF\u3002\u5B66\u4F1A\u5728Google Apps\u811A\u672C\u4E2D\u64CD\u7EB5\u8FD9\u4E9B\
  \u6570\u5B57\uFF0C\u80FD\u8BA9\u7A0B\u5E8F\u5458\u6269\u5C55\u4ED6\u4EEC\u5728\u79D1\
  \u5B66\u8BA1\u7B97\u3001\u4FE1\u53F7\u5904\u7406\u7B49\u65B9\u9762\u7684\u80FD\u529B\
  \u3002"
lastmod: '2024-03-13T22:44:47.192580-06:00'
model: gpt-4-0125-preview
summary: "\u590D\u6570\uFF0C\u4F5C\u4E3A\u5B9E\u90E8\u548C\u865A\u90E8\u7684\u7EC4\
  \u5408\u8868\u73B0\u5F62\u5F0F\uFF08\u4F8B\u5982\uFF0C3 + 4i\uFF09\uFF0C\u5728\u5404\
  \u79CD\u8BA1\u7B97\u95EE\u9898\u4E2D\u90FD\u662F\u57FA\u672C\u7684\uFF0C\u7279\u522B\
  \u662F\u5728\u5DE5\u7A0B\u3001\u7269\u7406\u548C\u5E94\u7528\u6570\u5B66\u7B49\u9886\
  \u57DF\u3002\u5B66\u4F1A\u5728Google Apps\u811A\u672C\u4E2D\u64CD\u7EB5\u8FD9\u4E9B\
  \u6570\u5B57\uFF0C\u80FD\u8BA9\u7A0B\u5E8F\u5458\u6269\u5C55\u4ED6\u4EEC\u5728\u79D1\
  \u5B66\u8BA1\u7B97\u3001\u4FE1\u53F7\u5904\u7406\u7B49\u65B9\u9762\u7684\u80FD\u529B\
  \u3002."
title: "\u5904\u7406\u590D\u6570\u7684\u5DE5\u4F5C"
weight: 14
---

## 什么 & 为什么？
复数，作为实部和虚部的组合表现形式（例如，3 + 4i），在各种计算问题中都是基本的，特别是在工程、物理和应用数学等领域。学会在Google Apps脚本中操纵这些数字，能让程序员扩展他们在科学计算、信号处理等方面的能力。

## 如何操作：
Google Apps脚本没有为复数提供内置支持，这就需要实现自定义功能。下面是处理复数的基本结构，包括加法、减法和乘法。

```javascript
// 定义一个复数的构造函数
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// 两个复数相加的方法
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// 两个复数相减的方法
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// 两个复数相乘的方法
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// 示例使用
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// 两个复数相加
var sum = num1.add(num2);
console.log(`和：${sum.real} + ${sum.imag}i`); // 和：4 + 6i

// 两个复数相减
var difference = num1.subtract(num2);
console.log(`差：${difference.real} + ${difference.imag}i`); // 差：2 + 2i

// 两个复数相乘
var product = num1.multiply(num2);
console.log(`积：${product.real} + ${product.imag}i`); // 积：-5 + 10i
```

## 深入了解：
复数的概念可以追溯到16世纪，但是是像欧拉和高斯这样的数学家的工作，巩固了它们在数学中的地位。尽管复数非常有用，但是在JavaScript或者说Google Apps脚本中并没有直接支持复数。缺乏原生支持意味着必须手动实现复数上的操作，正如所示。虽然这提供了一个很好的学习机会并且对于基本需求提供了足够的功能，但是对于需要使用复数进行重量级计算工作的情况，人们可能考虑利用更适合于数学计算的其他编程环境，例如带有NumPy的Python，它提供了用于处理复数的内置、高度优化的操作。然而，在Google Apps脚本中理解和实现基本操作，对于那些希望提升他们的编程技能并将其应用在广泛的场景中的人来说是一项有用的练习。
