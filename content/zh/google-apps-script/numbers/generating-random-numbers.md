---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:11.962863-07:00
description: "\u5982\u4F55\uFF1A \u5728 Google Apps \u811A\u672C\u4E2D\uFF0C\u4F60\
  \u53EF\u4EE5\u4F7F\u7528 `Math.random()` \u51FD\u6570\u751F\u6210\u968F\u673A\u6570\
  \u5B57\uFF0C\u7C7B\u4F3C\u4E8E JavaScript\u3002\u6B64\u51FD\u6570\u8FD4\u56DE\u4E00\
  \u4E2A\u5728 0\uFF08\u5305\u62EC\uFF09\u5230 1\uFF08\u4E0D\u5305\u62EC\uFF09\u8303\
  \u56F4\u5185\u7684\u6D6E\u70B9\u6570\u3001\u4F2A\u968F\u673A\u6570\u3002\u4E3A\u4E86\
  \u9488\u5BF9\u5404\u79CD\u7528\u4F8B\u5B9A\u5236\u8FD9\u4E9B\u6570\u5B57\uFF0C\u4F8B\
  \u5982\u751F\u6210\u7279\u5B9A\u8303\u56F4\u5185\u7684\u6574\u6570\uFF0C\u4F60\u53EF\
  \u80FD\u9700\u8981\u6267\u884C\u989D\u5916\u7684\u8BA1\u7B97\u3002 #."
lastmod: '2024-03-13T22:44:47.195501-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Google Apps \u811A\u672C\u4E2D\uFF0C\u4F60\u53EF\u4EE5\u4F7F\u7528\
  \ `Math.random()` \u51FD\u6570\u751F\u6210\u968F\u673A\u6570\u5B57\uFF0C\u7C7B\u4F3C\
  \u4E8E JavaScript\u3002\u6B64\u51FD\u6570\u8FD4\u56DE\u4E00\u4E2A\u5728 0\uFF08\u5305\
  \u62EC\uFF09\u5230 1\uFF08\u4E0D\u5305\u62EC\uFF09\u8303\u56F4\u5185\u7684\u6D6E\
  \u70B9\u6570\u3001\u4F2A\u968F\u673A\u6570\u3002\u4E3A\u4E86\u9488\u5BF9\u5404\u79CD\
  \u7528\u4F8B\u5B9A\u5236\u8FD9\u4E9B\u6570\u5B57\uFF0C\u4F8B\u5982\u751F\u6210\u7279\
  \u5B9A\u8303\u56F4\u5185\u7684\u6574\u6570\uFF0C\u4F60\u53EF\u80FD\u9700\u8981\u6267\
  \u884C\u989D\u5916\u7684\u8BA1\u7B97."
title: "\u751F\u6210\u968F\u673A\u6570"
weight: 12
---

## 如何：
在 Google Apps 脚本中，你可以使用 `Math.random()` 函数生成随机数字，类似于 JavaScript。此函数返回一个在 0（包括）到 1（不包括）范围内的浮点数、伪随机数。为了针对各种用例定制这些数字，例如生成特定范围内的整数，你可能需要执行额外的计算。

### 生成一个基本随机数
要生成一个简单的随机数并将其记录到控制台：

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*示例输出：* `0.1234567890123456`

### 在特定范围内生成整数
要生成两个值（`min` 和 `max`）之间的随机整数，包括两端值：

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// 示例：
getRandomInt(1, 10);
```
*示例输出：* `7`

记住，`Math.ceil()` 函数用于将最小值向上舍入，而 `Math.floor()` 用于将最大值向下舍入，以确保随机数在指定范围内。

## 深入探究
在 Google Apps 脚本中，以及实际上在大多数编程语言中，生成随机数的机制利用伪随机数生成器（PRNG）。这种技术是确定性的，依靠一个初始值，称为种子，以产生一系列看似随机的数字。虽然对于许多应用来说这已经足够，但重要的是要注意，伪随机数可能不适用于需要高安全性或真正随机性的场合，如在密码学应用中。

通过硬件随机数生成器或生成自然现象随机性的服务，可以实现真正的随机性。然而，对于 Google Apps 脚本中的大多数日常脚本需求，`Math.random()` 已经足够了。

从历史上看，对更有效的随机数生成技术的追求导致了各种算法的开发，著名的例子包括梅森旋转器和线性同余生成器（LCG）。然而，鉴于 Google Apps 脚本中的高度抽象，大多数用户不需要直接实现这些算法，但理解背后的原理有助于欣赏在你的脚本中随机数生成的重要性和局限性。
