---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么与为什么？  - What & Why?
生成随机数是计算机程序创建一个与预期输入无关的数字的过程。程序员生成随机数来进行测试，创建数据模拟等。

## 怎么做 - How to:
以下是在 TypeScript 中生成随机数的一种方式：

```TypeScript 
function getRandomInt(max: number): number {
    return Math.floor(Math.random() * Math.floor(max));
}
console.log(getRandomInt(1000));
```

此函数将生成一个从0（包含）到max（不包含）之间的随机整数，并使用 console.log() 打印在控制台。

## 深入探讨 - Deep Dive:
生成随机数的概念有着悠久的历史，它的最早形式可以追溯到古代的摇骰子和抽签。在计算机编程中，我们创建随机数以模拟这些现象。

有许多方式来生成随机数，上述的随机整数生成函数只是其中之一。也可以使用其他库，如 crypto 或者 lodash 来创建更复杂的随机数。

在计算机科学中，生成随机数的具体实现通常涉及一些形式的算法。这些算法的主要差别在于它们能够生成的随机数的范围和分布方式。

## 另请参阅 - See Also:
- [Math.random() - JavaScript | MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [lodash.random](https://lodash.com/docs/4.17.15#random)
- [crypto.randomInt](https://nodejs.org/api/crypto.html#crypto_crypto_randomint_min_max_callback)