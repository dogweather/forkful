---
title:                "生成随机数"
date:                  2024-01-20T17:50:30.164675-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
生成随机数就是让程序产出不可预测的数字。程序员用它来模拟、测试和加密。

## How to: (如何操作：)
在TypeScript中生成随机数，你可以用内置的`Math.random()`，这会给出一个0到1的浮点数。想要更大范围的数，就得自己计算一下。

```TypeScript
// 随机生成0到1之间的数字
const randomFraction: number = Math.random();
console.log(randomFraction);

// 随机生成一个0到100之间的整数
const randomInteger: number = Math.floor(Math.random() * 100);
console.log(randomInteger);

// 随机生成一个1到10之间的整数
const randomOneToTen: number = Math.floor(Math.random() * 10) + 1;
console.log(randomOneToTen);
```

要是看结果，就像这样：
```
0.4395067303071766   // 这一行和下面的会每次变
47                    // 每次运行都不同
8                     // 随机，每次都是1到10之间
```

## Deep Dive (深度探索)
随机数生成历史悠久，但计算机上的随机数实际上是“伪随机”的。它们依靠算法模拟随机性，这也意味着它们真正随机的话，就会有可预测的模式。否则，如果需要更强的随机性，如加密应用，可能会用到`crypto`模块的`crypto.getRandomValues()`。这个方法比`Math.random()`更安全、更不可预知。

```TypeScript
// 使用crypto生成一个安全的随机数
const buffer = new Uint32Array(1);
window.crypto.getRandomValues(buffer);
console.log(buffer[0]); // 随机产生一个32位无符号整数
```

## See Also (另请参阅)
- MDN Web Docs上的`Math.random()`: [MDN Math.random](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
- MDN Web Docs上的`crypto.getRandomValues()`: [MDN crypto.getRandomValues](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)
- TypeScript官方手册: [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)