---
title:                "生成随机数"
date:                  2024-01-20T17:49:36.015444-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? 什么和为什么？
生成随机数就是随机创建数字，好比掷骰子。程序员这么做，主要是为了让数据不可预测，用于游戏、安全性或者科学模拟。

## How to: 怎么做
JavaScript中生成随机数的基本方式是使用`Math.random()`。这个函数返回一个0到1之间的伪随机数。让我们看几个例子。

生成一个基本的随机数：
```javascript
console.log(Math.random()); // 输出：0.123456
```

要获得一个特定范围（比如1到100）的随机整数，可以这么做：
```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

console.log(getRandomInt(1, 100)); // 输出范围在 1 到 100 之间的一个整数
```

如果需要更复杂的随机事件，可能需要用到外部库，但这超出本文简介范围。

## Deep Dive 深入探讨
随机数生成有悠久历史。在计算机被发明之前，人类就通过各种方式产生随机性，比如掷骰子或摇号筒。计算机随机数通常是伪随机，意味着它们是通过算法产生的，其实是可以预测的，不过足够好的算法生成的随机数对于大多数应用来说都是够用的。

在某些情况下，比如密码学，需要真正的随机性，这时候可能会用到基于物理过程的硬件随机数生成器。

除了`Math.random()`之外，Node.js环境有`crypto`模块，它可以提供安全的随机数。浏览器的Web Cryptography API也有类似功能。

## See Also 参见
- [MDN Web Docs - Math.random()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [MDN Web Docs - Crypto.getRandomValues()](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
- [Wikipedia - Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)