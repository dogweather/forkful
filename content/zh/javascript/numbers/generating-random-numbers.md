---
title:                "生成随机数"
aliases:
- /zh/javascript/generating-random-numbers/
date:                  2024-01-27T20:34:24.070103-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 JavaScript 中生成随机数是一种用于在应用程序中创建不可预测性的技术，从需要随机敌人行为的游戏到需要密码学随机性的安全算法。这项能力对于开发动态用户体验和安全应用至关重要。

## 如何做：

### 基本随机数生成

在 JavaScript 中生成随机数最简单的方法是使用 `Math.random()`。这个函数返回一个在 0（包含）到 1（不包含）范围内的浮点数，伪随机数。

```javascript
let randomNumber = Math.random();
console.log(randomNumber);
```

### 生成指定范围内的随机数

通常，你会想要在特定范围内得到一个随机整数。通过缩放和四舍五入 `Math.random()` 的输出可以实现这一点。

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100));
```

### 密码学安全随机数

对于需要更高程度随机性的应用程序（例如，密码学操作），可以使用 `crypto.getRandomValues()` 方法。与 `Math.random()` 生成的伪随机数不同，这提供了密码学随机性。

```javascript
(function generateSecureRandom() {
  let array = new Uint32Array(1);
  window.crypto.getRandomValues(array);
  console.log(array[0]);
})();
```

## 深入探讨

历史上，在 JavaScript 中的随机数生成完全依赖于 `Math.random()` 函数。虽然这对大多数普通用例而言足够方便，但其算法通常是伪随机数生成器（PRNG）的变体，如 Mersenne Twister，并不提供密码学安全。

Web 密码学 API 的引入带来了 `crypto.getRandomValues()` 方法，提供了一种生成难以预测且适用于安全敏感应用的数字的方式。该方法利用底层操作系统的随机性源，如 Unix/Linux 上的 `/dev/random`，这些源更加健壮且适合密码学操作。

选择正确的方法对于手头的任务至关重要。`Math.random()` 对于基本需求，如简单游戏、动画或任何随机性质量不是关键的情况下足够好。然而，对于安全功能，如密码重置令牌或任何密码学操作，由于其优越的随机性质量，`crypto.getRandomValues()` 是更好的选择。

值得注意的是，`Math.random()` 在大多数实现中生成具有已知偏差的数字，意味着某些数字比其他数字出现的可能性更大。即使这种偏差很小且通常对于一般应用来说几乎察觉不到，它也使得 `Math.random()` 不能用于任何密码学上下文或公平性至关重要的应用，如在线赌博。

总之，虽然 JavaScript 的内置函数用于生成随机数可以满趀广泛的需求，但理解每种方法的差异和限制对于其适当应用至关重要。
