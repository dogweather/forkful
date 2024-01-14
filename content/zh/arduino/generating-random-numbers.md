---
title:    "Arduino: 生成随机数"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

<div id="why"></div>

## 为什么

生成随机数可能听起来并不是很有用，但它实际上在编程中有很多实际的应用。比如，你可以用它来创建抽奖程序、游戏随机事件或者测试程序。这篇博文将向你展示如何在Arduino中生成随机数，以及一些关于随机数的深入知识。

<div id="how-to"></div>

## 如何生成随机数

要在Arduino中生成随机数，你需要使用`random()`函数。这个函数接受两个参数：最小值和最大值。比如，如果你想要生成一个1-10之间的随机数，你可以使用以下代码：

```Arduino
int randomNumber = random(1, 11);
```

要注意的是，`random()`函数只能生成整数类型的随机数。如果你想要生成小数类型的随机数，你可以将`random()`函数的输出除以一个小数，比如`10.0`。

```Arduino
float randomNumber = random(1, 11) / 10.0;
```

Arduino也提供了一些其他的函数来帮助你生成随机数，比如`randomSeed()`函数用于设置随机数的种子，`randomBytes()`函数用于生成随机字节。更多关于这些函数的信息可以参考Arduino官方文档。

<span id="deep-dive"></span>

## 深入探究随机数

随机数的本质是不可预测性，它是通过随机事件来生成的。在计算机中，这个随机事件是通过一个种子(seed)值来确定的。种子值是一个数字，它作为一个起点来生成随机数序列。如果你使用的是同样的种子值，你将会得到同样的随机数序列。

这就是为什么我们在生成随机数时需要使用`randomSeed()`函数来提供一个随机的种子值。通常，我们使用当前时间作为种子值来产生更加随机的序列。你也可以使用其他特殊的种子值来产生特定的序列，比如使用Arduino板上的电量读数。

随机数也是伪随机的，它们实际上是通过一个算法来生成的，而不是真正的随机事件。这就意味着，如果你使用相同的种子值，你将会得到相同的随机数序列。但是，对于大多数应用来说，伪随机数已经足够随机。

<div id="see-also"></div>

## 参考链接

- [Arduino官方文档](https://www.arduino.cc/reference/en/language/functions/random-numbers/)
- [Java随机数生成原理](https://www.javatpoint.com/how-to-generate-random-number-in-java) (注意：Arduino使用的随机数生成算法可能不同于Java)