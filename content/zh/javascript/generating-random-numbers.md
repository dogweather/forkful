---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？
生成随机数是编程中产生不可预测数字的过程。程序员使用它来创建随机事件，比如抽奖应用或者在游戏中模拟不可预知的行为。

## 如何操作：
在Javascript中，你可以使用`Math.random()`来生成0到1之间的伪随机浮点数。看下面的例子：

```Javascript
let randomNumber = Math.random();
console.log(randomNumber);  
```

在这段代码中，我们调用了Math.random()函数，它返回了一个在0和1之间的随机数（包含0，但不包含1）。每次你运行代码，`randomNumber`都会是一个新的随机数字。

如果你需要在一个特定范围内生成随机数（比如在1和100之间），你可以这样写：

```Javascript
let min = 1;
let max = 100;
let randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
console.log(randomNumber);
```

这段代码会返回1到100之间的一个整数。每次运行时，`randomNumber`都会是这个范围内的一个新随机数。

## 深度解析：
生成随机数的历史可以追溯到计算机科学的早期。先前的方法如使用电机噪声，采用钟摆摆动等，都被认为是产生真正随机数字的有效方式。但是，这些方法都不适用于计算机编程。

对于编程来说，最常用的是伪随机数生成器 (PRNGs)。JavaScript使用称为“线性同余法”的PRNG。一个被选定的初始数字（称为“种子”），根据预设的一组数学方程（称为“算法”）进行运算，生成的结果看起来像随机数，这就是伪随机。

虽然`Math.random()`对于大部分用途都很足够，但如果你需要特别的公正或安全性，它可能不够。这种情况下，你可能需要寻求其他更为深度的随机数生成想要实现。

## 另请参阅：
- MDN 文档对JavaScript中`Math.random()`的解释 (https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- Node.js的`crypto.randomInt()`作为具有更高安全性的随机数生成的备选方法(https://nodejs.org/api/crypto.html#crypto_crypto_randomint_min_max_callback)
- "用JavaScript制作随机数"的详细文章. (https://www.javascripting.org/creating-random-numbers/)