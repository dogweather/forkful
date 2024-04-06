---
date: 2024-01-26 03:47:10.707340-07:00
description: "\u5982\u4F55\u505A\uFF1A \u5728TypeScript\u4E2D\u53EF\u4EE5\u4F7F\u7528\
  \u51E0\u79CD\u65B9\u6CD5\u8FDB\u884C\u56DB\u820D\u4E94\u5165\u3002\u8FD9\u91CC\u5FEB\
  \u901F\u4ECB\u7ECD\u4E00\u4E0B\uFF1A."
lastmod: '2024-04-05T21:53:47.790609-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

## 如何做：
在TypeScript中可以使用几种方法进行四舍五入。这里快速介绍一下：

```typescript
// Math.round 四舍五入到最近的整数
console.log(Math.round(1.5)); // 输出：2

// Math.ceil 向上舍入到最近的整数
console.log(Math.ceil(1.1)); // 输出：2

// Math.floor 向下舍入到最近的整数
console.log(Math.floor(1.8)); // 输出：1

// toFixed 四舍五入到固定的小数位数
let num = 1.23456;
console.log(num.toFixed(2)); // 输出："1.23"
// 注意：toFixed返回一个字符串！如果需要，使用parseFloat进行转换。
console.log(parseFloat(num.toFixed(2))); // 输出：1.23
```

## 深入探索
早期，由于计算机的空间和精度限制，四舍五入是必需的。今天，由于数字以二进制形式存储，浮点算术可能导致奇怪的结果。四舍五入的替代方法包括floor、ceil和trunc（用于截去小数部分而不四舍五入）。

内部机制值得注意：`Math.round` 遵循“四舍五入”规则（又称“商业舍入”），而`Math.floor`和`Math.ceil`则相对直接。`toFixed`可能会导致意外的结果，因为它返回一个字符串，并且它使用“四舍五入到偶数”规则（又称“银行家舍入法”），特别适用于减少多次四舍五入同一数字时的偏差。

## 另请参见
- [MDN - Math.round()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/round)
- [MDN - Math.ceil()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/ceil)
- [MDN - Math.floor()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/floor)
- [MDN - toFixed()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Number/toFixed)
- [IEEE浮点算术标准（IEEE 754）](https://ieeexplore.ieee.org/document/4610935)
