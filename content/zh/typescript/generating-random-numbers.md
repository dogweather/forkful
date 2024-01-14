---
title:                "TypeScript: 生成随机数"
simple_title:         "生成随机数"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

随机数在编程中扮演着重要的角色，它可以用来模拟随机事件，生成随机密码，以及用于游戏等各种应用场景。生成随机数可以让你的程序更加灵活，增加趣味性，让用户有不同的体验。在TypeScript中，我们可以轻松地生成随机数并应用于不同的项目中。

## 如何做

通过使用TypeScript内置的Math对象中的函数，我们可以轻松地实现随机数的生成。首先，我们需要声明一个变量来接收随机数，然后通过Math对象中的函数来生成随机数，最后将其打印出来。下面是一个简单的例子：

```TypeScript
let randomNum = Math.random();
console.log(randomNum);
```

这段代码会生成一个0到1之间的随机小数，并将其打印在控制台上。如果你想要生成指定范围内的随机整数，可以使用Math.floor()函数将随机小数取整，然后再乘以范围值。下面是一个生成1到10之间的随机整数的示例：

```TypeScript
let randomInt = Math.floor(Math.random() * 10) + 1;
console.log(randomInt);
```

这样就可以生成一个1到10之间的随机整数。除了Math.random()和Math.floor()外，Math对象中还有其他用于生成随机数的函数，比如Math.round()用于四舍五入，Math.ceil()用于向上取整，Math.trunc()用于去除小数部分等等。通过组合使用这些函数，我们可以实现更多有趣的随机数生成。

## 深入探讨

虽然我们在上面提到了使用Math对象中的函数来生成随机数，但是这些函数并不是真正意义上的“随机”，而是伪随机。它们根据一个种子值来生成一系列看起来随机的结果，但是这些结果实际上是可以预测的。所以在一些安全性要求较高的场景，我们需要使用专门的加密类来生成真正意义上的随机数。

此外，在生成大量随机数时，为了避免重复出现相同的值，我们可以使用shuffle算法来打乱已有的数组，然后取出打乱后的第一个值作为随机数。这样可以更有效地生成独特的随机数，避免出现重复。

## 参考链接

- [TypeScript官方文档：Math对象](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-0.html#new-built-in-types-math)
- [真正随机数生成器的介绍](https://stackify.com/random-number-generator/)
- [用JavaScript实现Fisher-Yates shuffle算法](https://www.javascripttutorial.net/array/javascript-shuffle-array/)