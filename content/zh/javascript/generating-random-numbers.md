---
title:                "生成随机数"
html_title:           "Javascript: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

随机数生成是一种非常有趣和常用的技能。它可以用于创建游戏、测试程序或模拟任何随机事件。无论您是一名初学者还是一名专业人士，了解如何在Javascript中生成随机数都是具有实用性的。

## 如何编码

生成随机数是一个简单但又有趣的过程，我们可以使用Math对象中的内置函数来完成。下面是使用Javascript代码如何生成一个1到10之间的随机整数的例子：

```Javascript
let randomNum = Math.floor(Math.random() * 10) + 1;
console.log(randomNum); // Output: 7
```

让我们来解释一下这段代码。首先，我们使用`Math.random()`函数来生成一个0到1之间的随机小数。为了将它转换为整数，我们使用`Math.floor()`函数舍去小数部分。然后，我们将结果与10相乘，得到一个0到10之间的数。最后，我们再加上1，这样我们就得到了一个1到10之间的随机整数。

除了生成整数，我们也可以使用相同的原理来生成特定范围内的随机小数。例如，如果我们想要生成一个0到1之间的小数，我们只需要使用`Math.random()`函数即可。如果我们想要生成一个1到100之间的小数，我们可以使用以下代码：

```Javascript
let randomNum = Math.random() * 100;
console.log(randomNum); // Output: 74.253811587
```

还有另一种方法可以生成指定范围内的随机整数，那就是使用`Math.floor()`函数和`Math.random()`函数的结合。以下是一个生成1到5之间的随机整数的示例：

```Javascript
let randomNum = Math.floor(Math.random() * 5) + 1;
console.log(randomNum); // Output: 3
```

## 深入了解

生成随机数并不意味着完全的随机性。Javascript中内置的`Math.random()`函数实际上是基于伪随机数生成器算法（PRNG），它使用一个称为"种子"的输入来生成随机数。默认情况下，`Math.random()`函数的种子是当前的系统时间，因此每次运行代码时它都会生成不同的随机数。

了解PRNG的工作原理对于生成不一样的随机数是很重要的。例如，如果我们想要在游戏中使用随机数，那么每个玩家应该获得不同的数。为了做到这一点，我们可以使用不同的种子来调用`Math.random()`函数。

另一个需要注意的地方是PRNG的周期性。周期性是指生成的随机数序列会在一定的次数后重复。对于大多数应用来说，周期性并不是一个问题，但是对于安全性要求高的应用来说，我们可能需要使用更加复杂的随机数生成算法。

## 参考链接

- [MDN Web Docs: Math.random()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [深入理解伪随机数序列](http://adsabs.harvard.edu/full/1999ASPC..172...25G)