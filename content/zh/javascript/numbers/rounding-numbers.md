---
date: 2024-01-26 03:45:46.308701-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u4EE5\u4E0B\u662F\u5728JavaScript\u4E2D\
  \u4F7F\u7528`Math.round()`\u3001`Math.ceil()`\u548C`Math.floor()`\u8FDB\u884C\u6570\
  \u5B57\u820D\u5165\u7684\u65B9\u6CD5\uFF1A."
lastmod: '2024-04-05T21:53:48.487845-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6570\u5B57\u53D6\u6574"
weight: 13
---

## 如何操作：
以下是在JavaScript中使用`Math.round()`、`Math.ceil()`和`Math.floor()`进行数字舍入的方法：

```javascript
let originalNumber = 2.567;

let roundedDown = Math.floor(originalNumber); // 2
let roundedUp = Math.ceil(originalNumber);    // 3
let rounded = Math.round(originalNumber);     // 3（因为.567大于.5）

console.log(roundedDown); // 打印：2
console.log(roundedUp);   // 打印：3
console.log(rounded);     // 打印：3
```

要固定到特定的小数位数，请使用`toFixed()`：

```javascript
let twoDecimals = originalNumber.toFixed(2); // "2.57" （返回一个字符串）

console.log(twoDecimals); // 打印："2.57"
```

使用一元加号或`Number()`将字符串重新转换为数字：

```javascript
let numberAgain = +twoDecimals; // 2.57

console.log(numberAgain); // 打印：2.57
```

## 深入探讨
数字舍入并不是什么新鲜事；它和数字一样古老。在JavaScript中，`Math.round()`使用“四舍五入”规则：如果小数部分是0.5，就舍入到最近的偶数。

想要更多控制力，`toFixed()`可能是你的首选，但记住，它返回一个字符串。重新转换回数字可能是一个额外的步骤，但确保你继续使用数字类型工作。

有其他选择吗？像`lodash`这样的库提供了`_.round(number, [precision=0])`，用于更细致的控制。或者，较新的`Intl.NumberFormat`提供了远超舍入的高精度格式化。

说到精度，要小心JavaScript中浮点数的怪癖。`0.1 + 0.2`并不严格等于`0.3`，这是因为数字的存储方式。有时，舍入变得必要，以纠正这种浮点数错误。

## 另请参阅
- Mozilla的数学文档：[MDN Web文档](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Math)
- 使用`Intl.NumberFormat`进行财务舍入：[ECMAScript国际化API](https://tc39.es/ecma402/#numberformat-objects)
- `lodash`舍入：[Lodash文档](https://lodash.com/docs/4.17.15#round)
