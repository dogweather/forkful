---
title:                "产生随机数字"
html_title:           "Go: 产生随机数字"
simple_title:         "产生随机数字"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

你曾经需要在代码中生成随机数吗？也许你需要为测试目的生成随机数据，或者设计一个游戏，或者需要一个随机的加密密钥。无论出于何种原因，生成随机数是在编程中很常见的任务，Go语言提供了一些方便的方法来实现这一目的。

## 如何做 

下面是一个使用Go语言生成随机数的简单示例，它将生成10个介于0和100之间的随机整数：

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())

	for i := 0; i < 10; i++ {
		fmt.Println(rand.Intn(100))
	}
}
```

运行该程序，你将会得到类似下面的输出：

```
7
45
83
96
22
73
63
14
93
58
```

为了能够生成不同的随机数，我们使用了`rand.Seed()`函数来设置随机数种子。种子通常使用当前时间的纳秒数来确保每次运行程序时，都能得到不同的随机数序列。然后，我们使用`rand.Intn()`函数来生成介于0和给定参数之间的随机整数。

除了`Intn()`函数，Go语言还提供了一系列用于生成随机数的函数，包括生成随机浮点数和随机字符串。你可以阅读Go语言官方文档来进一步了解这些函数的用法。

## 深入探讨

在生成随机数时，我们需要关注到的一个重要问题就是随机数的真实性。使用种子值来生成随机数的方法虽然随机性较高，但是仍然不是完全随机的。因此，在进行安全加密或随机算法时，不能仅仅依赖于随机数种子来保证安全性，而需要使用更加复杂的随机数生成方法。如果你对随机数的生成算法感兴趣，可以进一步学习随机数伪随机性和真随机性的知识。

## 参考链接

- [Go语言官方文档](https://golang.org/pkg/math/rand/)
- [随机数的真实性](https://en.wikipedia.org/wiki/Randomness)
- [随机数种子设置方法](https://golang.org/pkg/math/rand/#Seed)