---
title:    "Javascript: 生成随机数"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/javascript/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么会生成随机数

在编程中，随机数是一个非常有用的工具。它可以被用来模拟随机事件，或者帮助生成唯一的标识符。使用随机数也可以增加程序的安全性，防止被恶意攻击。

# 如何生成随机数

在Javascript中，可以通过`Math.random()`函数来生成一个0到1之间的随机数。如果需要生成指定范围内的随机数，可以使用以下公式：

```
Math.floor(Math.random() * (max - min + 1) + min);
```

其中`max`和`min`分别为所需要的随机数范围的最大值和最小值。例如，如果我们需要生成1到10之间的随机整数，可以使用以下代码：

```
let randomNumber = Math.floor(Math.random() * (10 - 1 + 1) + 1);
console.log(randomNumber);
```

以上代码的输出可能为`7`、`2`、`10`等随机整数。

# 深入了解随机数生成

在计算机中，真正的随机数是不存在的，因为计算机是通过算法来生成随机数的。 `Math.random()`函数其实是基于一个伪随机数算法来实现的，所以它生成的随机数在一定程度上是有规律的。如果要生成更加随机的数，可以使用外部的随机数生成器来获取随机种子，然后再进行计算。

# 参考链接

- [了解Javascript中的随机数生成](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- [Javascript生成指定范围内随机数的方法](https://www.cnblogs.com/ldp2016/p/6066837.html)
- [计算机生成随机数的原理](https://www.eecis.udel.edu/~boothe/eleg601/randomnumgen.pdf)

## 参见

- [Markdown语法参考](https://www.markdownguide.org/basic-syntax/)
- [Javascript官方文档](https://developer.mozilla.org/zh-CN/docs/Web/JavaScript)