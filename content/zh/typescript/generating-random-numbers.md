---
title:                "TypeScript: 生成随机数"
programming_language: "TypeScript"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么生成随机数
如今，随机数在计算机编程中变得越来越重要。它们可以用来创建随机游戏，保护密码，做数据分析等等。生成随机数可以增加程序的难度和安全性，保证每次运行程序都能得到不同的结果。

## 如何生成随机数
```TypeScript
// 导入Math库
import { Math } from "tsmath";

// 生成0-1之间的随机数
let randomNum = Math.random();

// 生成指定范围内的随机整数
let randomInt = Math.floor(Math.random() * 10) + 1;
```

输出示例:
```
randomNum = 0.547
randomInt = 8
```

## 深入探讨随机数生成
生成随机数的方法有很多种，每种方法都有其优缺点。常用的方法包括伪随机数生成器和真随机数生成器。伪随机数生成器基于一个初始值，之后每次运行都会按照确定的步骤生成相同的序列。而真随机数生成器则利用外部因素如游戏中的骰子或物理现象来产生真正的随机数。

另外，在生成随机数时也要注意其分布情况，以保证数值的均匀性。一般来说，我们希望生成的随机数能够近似于均匀分布，这样才能更好地实现随机的效果。

## 查看更多资料
- [Math库文档](https://www.typescriptlang.org/docs/handbook/declaration-files/by-example/library.html)
- [伪随机数生成器和真随机数生成器的比较](https://www.geeksforgeeks.org/pseudo-random-vs-true-random-number-generator/)
- [如何生成均匀分布的随机数](https://www.quora.com/What-is-the-best-way-to-generate-uniformly-distributed-random-numbers)

## 参考资料
- [Random Number Generation in TypeScript](https://blog.bitsrc.io/random-number-generation-in-typescript-7633d550e671)
- [The Importance of Randomness in Programming](https://medium.com/@olenicksoftware/the-importance-of-randomness-in-programming-719d0346e3ac)
- [The Art of Randomness in Programming](https://www.techrepublic.com/blog/software-engineer/the-art-of-randomness-in-programming/)