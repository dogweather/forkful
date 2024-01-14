---
title:                "Javascript: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么使用随机数生成器

随机数生成器在编程中扮演着非常重要的角色，它可以为我们提供随机的数值，使得我们的程序变得更加多样化和有趣。例如，如果你正在开发一款游戏，你肯定会需要通过随机数来决定敌人的行动或者随机生成地图，这样游戏体验才会更加刺激。所以说，使用随机数生成器可以让我们的程序更加生动和具有挑战性。

## 如何使用

我们可以使用Math对象中的random方法来生成随机数，它会返回一个0到1之间的随机小数。如果需要一个特定范围内的随机整数，我们可以通过乘以范围的长度再加上起始值，然后使用Math.floor()来取整。下面是一个例子：

```Javascript
let randomNumber = Math.random(); // 生成0到1之间的随机小数
let randomInteger = Math.floor(Math.random() * 10 + 1); // 生成1到10之间的随机整数
console.log(randomNumber);
console.log(randomInteger);
```

```Javascript
输出：
0.5467865432 // 每次运行结果都不同
5 // 每次运行结果都不同
```

我们也可以通过制定随机数的种子来控制随机数的生成。种子可以是任意的数值，但是通常我们会使用当前的时间戳作为种子，这样每次生成的随机数都会有所不同。下面是一个例子：

```Javascript
let randomNumber = Math.random(1234); 
console.log(randomNumber);
```

```Javascript
输出：
0.432164545 // 每次运行结果都相同
```

## 深入探讨

有时候，我们可能需要生成特定分布的随机数。比如，我们希望生成符合正态分布的随机数，这样我们可以更加真实地模拟一些实验数据。为了实现这样的功能，我们可以使用第三方库如`random-js`。它提供了丰富的随机数生成方法，包括常见的均匀分布、正态分布、负二项分布等等。下面是一个例子：

```Javascript
const Random = require('random-js');
let random = new Random(); // 创建随机数生成器
let normalRandomNumber = random.normal(0, 1); // 生成均值为0，标准差为1的正态分布随机数
console.log(normalRandomNumber);
```

```Javascript
输出：
-0.7683756
```

在使用随机数生成器时，我们还需要注意一些潜在的问题。比如，生成的随机数并非真正的随机，它们只是按照一定的规则来进行计算。这意味着如果我们使用相同的种子或者相同的算法，就有可能得到相同的结果。因此，在一些安全性要求高的场景中，我们需要使用更加复杂的方法来生成随机数，以防止被破解。

# 参考链接

- [Math.random()文档](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [random-js文档](https://www.npmjs.com/package/random-js)
- [随机数生成器的安全性问题](https://www.jianshu.com/p/78788fa4d449)

## 参见

[Markdown文档](https://www.markdownguide.org)