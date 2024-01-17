---
title:                "产生随机数"
html_title:           "Javascript: 产生随机数"
simple_title:         "产生随机数"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

机器具有计算能力，但是在某些任务中，重复性是必要的。为了解决这个问题，程序员们创建了一种方法来使用计算机来生成随机数。这就是我们今天要探讨的主题-生成随机数。

## 什么 & 为什么?

生成随机数是一种使用计算机来产生看似随机的数字的方法。这在许多应用中都很有用，例如游戏中的随机事件，密码生成以及模拟实验。

程序员们经常需要生成随机数来进行测试和调试。它也可以被用来作为安全功能，例如在密码重置过程中生成临时密码。

## 如何:

下面是一个简单的例子来生成一个范围在1到10之间的随机数：

```javascript
Math.floor(Math.random() * 10) + 1;
```

这里我们使用了`Math.random()`方法来生成0到1之间的随机小数。然后通过乘以10来扩大范围，并使用`Math.floor()`方法来将小数转换为整数。最后，加上1来将范围设置为1到10。

我们也可以将这个逻辑封装到一个函数中，让我们可以重复使用它：

```javascript
function getRandomNumber(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// 生成一个范围在3到8之间的随机数
getRandomNumber(3, 8);
```

## 深入探讨:

在早期的计算机中，生成随机数并不容易。因为计算机在其本质上是可预测的，它必须从一个种子开始，然后通过一系列的计算来生成随机数。现代计算机使用的伪随机数生成器，通过一系列复杂的算法来生成看似随机的数字。

除了使用`Math.random()`方法来生成随机数，还有其他一些方法，如使用随机数生成器软件或使用硬件设备来生成真正的随机数。

## 看看这些:

- [JavaScript随机数生成器](https://javascript.info/random) ：一个详细的教程来学习如何在JavaScript中生成随机数。
- [随机数生成器](https://www.random.org/) ：一个可以生成真正随机数的在线工具。
- [利用随机数生成器提高密码强度](https://www.cnet.com/news/using-a-random-number-generator-to-improve-password-security/)：一个关于如何使用随机数生成器来提高密码安全性的文章。