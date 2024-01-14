---
title:    "Javascript: 生成随机数"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## 为什么要生成随机数

生成随机数在编程中是一项非常常见的任务。它可以被用来创建测试数据，增加程序的随机性，以及模拟真实世界中的随机事件。同时，生成随机数也是一种很有趣的编程挑战，可以帮助提高编程技能。无论你是初学者还是有经验的程序员，学习如何生成随机数都是很有价值的。

## 如何生成随机数

在Javascript中，我们可以使用内置的`Math.random()`函数来生成0到1之间的随机数。下面是一个简单的示例，它将生成和打印5个随机数：

```Javascript
for (let i = 0; i < 5; i++) {
  let randomNum = Math.random();
  console.log(randomNum);
}
// Output: 
// 0.2645981940804652
// 0.7238975991683529
// 0.15641965144393282
// 0.8976882373140053
// 0.47038710399253965
```

如果我们想要生成一个特定范围内的随机数，可以使用`Math.random()`乘以一个数字，并再加上另一个数字。例如，以下代码将生成和打印1到10之间的随机整数：

```Javascript
let randomInt = Math.floor(Math.random() * 10) + 1;
console.log(randomInt);
// Output: 7
```

除了`Math.random()`函数，我们也可以使用其他的Javascript库或第三方API来生成随机数。比如，可以使用`faker.js`来生成更复杂的随机数据，或使用`random.org`来获取真正的随机数。

## 深入了解随机数

生成的随机数实际上并不是完全随机的。它们是由计算机算法生成的伪随机数。因此，每次运行同样的代码，都会得到相同的随机数序列。为了避免这种情况，我们可以使用一个种子（seed）来初始化随机数生成器。种子是一个数字，它可以改变生成的随机数序列。在Javascript中，我们可以使用`Math.seedrandom()`库来实现这一点。

另外，随机数也可以被用来帮助解决一些数学难题，比如在Monte Carlo方法中，通过生成大量随机数来近似计算概率和积分。因此，在学习如何生成随机数时，也可以深入研究相关的数学和计算方法。

## 参考资料

- [MDN web docs：随机数生成器](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [W3School：生成随机数](https://www.w3schools.com/js/js_random.asp)
- [全球真随机数：random.org](https://www.random.org/)
- [本文中使用的库：seedrandom](https://github.com/davidbau/seedrandom)

## 参见

- [MDN web docs：Math.random()](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [W3School：Math.random()](https://www.w3schools.com/js/js_random.asp)