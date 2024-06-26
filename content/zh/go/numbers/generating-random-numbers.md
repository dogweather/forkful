---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:57:30.808237-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Go\u8BED\u8A00\u4E2D\uFF0C\u968F\
  \u673A\u6570\u7684\u751F\u6210\u662F\u901A\u8FC7\u4F7F\u7528`math/rand` \u5305\u6765\
  \u751F\u6210\u4F2A\u968F\u673A\u6570\u6216\u8005\u4F7F\u7528 `crypto/rand` \u6765\
  \u751F\u6210\u52A0\u5BC6\u5B89\u5168\u7684\u4F2A\u968F\u673A\u6570\u3002\u6211\u4EEC\
  \u6765\u63A2\u7D22\u8FD9\u4E24\u8005\u3002"
lastmod: '2024-04-05T22:38:46.325076-06:00'
model: gpt-4-0125-preview
summary: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728Go\u8BED\u8A00\u4E2D\uFF0C\u968F\u673A\
  \u6570\u7684\u751F\u6210\u662F\u901A\u8FC7\u4F7F\u7528`math/rand` \u5305\u6765\u751F\
  \u6210\u4F2A\u968F\u673A\u6570\u6216\u8005\u4F7F\u7528 `crypto/rand` \u6765\u751F\
  \u6210\u52A0\u5BC6\u5B89\u5168\u7684\u4F2A\u968F\u673A\u6570\u3002\u6211\u4EEC\u6765\
  \u63A2\u7D22\u8FD9\u4E24\u8005\u3002"
title: "\u751F\u6210\u968F\u673A\u6570"
weight: 12
---

## 如何操作：
在Go语言中，随机数的生成是通过使用`math/rand` 包来生成伪随机数或者使用 `crypto/rand` 来生成加密安全的伪随机数。我们来探索这两者。

### 使用 `math/rand` 生成伪随机数
首先，引入 `math/rand` 包和 `time` 包来给生成器种子。种子确保您每次运行都能得到不同的数字序列。

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	fmt.Println("一个随机数:", rand.Intn(100)) // 生成一个0到99之间的数字
}
```

示例输出：`一个随机数: 42`

### 使用 `crypto/rand` 生成加密安全伪随机数
对于更敏感的安全应用，`crypto/rand` 包是合适的，因为它生成的随机数难以预测，使它们适合加密操作。

```go
package main

import (
	"crypto/rand"
	"fmt"
	"math/big"
)

func main() {
	n, _ := rand.Int(rand.Reader, big.NewInt(100))
	fmt.Println("一个安全的随机数:", n)
}
```

示例输出：`一个安全的随机数: 81`

## 深入了解
`math/rand` 和 `crypto/rand` 在Go中的核心区别源自它们的熵源和预定使用场景。`math/rand` 基于一个初始种子生成伪随机数；因此，如果种子已知，序列是确定性的并且可以被预测。这适用于高性能而非绝对不可预测性是关键关注点的场景，如模拟或游戏。

另一方面，`crypto/rand` 派生自底层操作系统的随机性，使其适用于不可预测性至关重要的加密用途。然而，这以性能和复杂性为代价（比如处理生成的数字时需要处理 `*big.Int` 类型的整数）。

从历史上看，计算机中随机数生成的概念一直在真正的“随机性”边缘徘徊，早期系统严重依赖于模仿随机性的确定性算法。随着计算机的发展，这些算法也随之进化，从它们的环境中融入了更复杂的熵源。

尽管有这些进步，鉴于计算机本身的确定性本质，对计算中完美随机性的追求本质上是矛盾的。这就是为什么，对于大多数预测性可能会有害的应用，来自 `crypto/rand` 等源的加密安全伪随机数是更好的选择，尽管它们有开销。

本质上，Go通过两个不同的包来处理随机数生成，优雅地解决了性能与安全之间的折衷，允许开发人员根据他们的特定需要进行选择。
