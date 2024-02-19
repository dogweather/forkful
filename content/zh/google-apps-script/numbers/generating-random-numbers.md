---
aliases:
- /zh/google-apps-script/generating-random-numbers/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:11.962863-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\u751F\u6210\u968F\u673A\u6570\u5B57\u662F\u4E00\
  \u9879\u57FA\u672C\u4EFB\u52A1\uFF0C\u7528\u4E8E\u4F17\u591A\u5E94\u7528\u4E2D\uFF0C\
  \u5982\u6A21\u62DF\u3001\u6E38\u620F\u548C\u5B89\u5168\u7CFB\u7EDF\u3002\u7A0B\u5E8F\
  \u5458\u5728 Google Apps \u811A\u672C\u4E2D\u4F7F\u7528\u8FD9\u79CD\u6280\u672F\u6765\
  \u5F15\u5165\u53EF\u53D8\u6027\u3001\u6D4B\u8BD5\u573A\u666F\uFF0C\u5E76\u5411\u4ED6\
  \u4EEC\u5728 Google \u751F\u6001\u7CFB\u7EDF\u5185\u7684\u5E94\u7528\u7A0B\u5E8F\
  \uFF08\u5305\u62EC Sheets\u3001Docs \u548C Forms\uFF09\u6DFB\u52A0\u4E0D\u53EF\u9884\
  \u6D4B\u6027\u3002"
lastmod: 2024-02-18 23:08:58.745837
model: gpt-4-0125-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\u751F\u6210\u968F\u673A\u6570\u5B57\u662F\u4E00\
  \u9879\u57FA\u672C\u4EFB\u52A1\uFF0C\u7528\u4E8E\u4F17\u591A\u5E94\u7528\u4E2D\uFF0C\
  \u5982\u6A21\u62DF\u3001\u6E38\u620F\u548C\u5B89\u5168\u7CFB\u7EDF\u3002\u7A0B\u5E8F\
  \u5458\u5728 Google Apps \u811A\u672C\u4E2D\u4F7F\u7528\u8FD9\u79CD\u6280\u672F\u6765\
  \u5F15\u5165\u53EF\u53D8\u6027\u3001\u6D4B\u8BD5\u573A\u666F\uFF0C\u5E76\u5411\u4ED6\
  \u4EEC\u5728 Google \u751F\u6001\u7CFB\u7EDF\u5185\u7684\u5E94\u7528\u7A0B\u5E8F\
  \uFF08\u5305\u62EC Sheets\u3001Docs \u548C Forms\uFF09\u6DFB\u52A0\u4E0D\u53EF\u9884\
  \u6D4B\u6027\u3002"
title: "\u751F\u6210\u968F\u673A\u6570"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在编程中生成随机数字是一项基本任务，用于众多应用中，如模拟、游戏和安全系统。程序员在 Google Apps 脚本中使用这种技术来引入可变性、测试场景，并向他们在 Google 生态系统内的应用程序（包括 Sheets、Docs 和 Forms）添加不可预测性。

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
