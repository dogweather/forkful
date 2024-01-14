---
title:                "Javascript: 产生随机数"
programming_language: "Javascript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

为什么：生成随机数在编程中的重要性
生成随机数在许多编程任务中都非常有用。它们可以帮助我们模拟真实的现实情况，测试我们的代码以及为游戏或随机选择做出决策。

## 如何：
在Javascript中生成随机数非常简单。我们可以使用Math对象的random()方法来生成一个从0到1之间的随机数。然而，如果我们想要生成一个特定范围内的随机整数，我们可以结合使用random()方法和Math.floor()方法。下面是一个例子：

```Javascript
// 生成0到10之间的随机整数
let randomNum = Math.floor(Math.random() * 11);
console.log(randomNum);
// 输出：在0到10之间的随机整数，例如7
```

## 深入探讨：
在生成随机数时，我们应该注意一些潜在的隐患。随机数并不是真正的“随机”，它们实际上是由一个算法生成的伪随机数。因此，在某些情况下，我们可能会得到相同的随机数。为了避免这种情况，我们可以使用一个种子值来初始化随机数生成器，这样我们每次都可以得到不同的随机数序列。

此外，当生成随机数时，我们应该尽量避免在循环中使用随机数，因为它们会导致性能下降。我们应该在循环外部生成随机数，并在循环内部使用变量来存储它们。

## 参考链接：

- [Javascript中的随机数生成器](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [掘金：随机数生成器的深入探讨](https://juejin.im/post/5de095a1e51d451aec1576e2)
- [掘金：如何正确地生成随机数](https://juejin.im/post/5d581f40f265da03c7219f89)

## 参见：
参考链接和其他有关随机数生成的文章。