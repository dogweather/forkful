---
title:                "生成随机数"
date:                  2024-01-20T17:49:16.586609-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? 为何以及为什么？
生成随机数就是创建不可预测的数字序列。 程序员用它来测试程序、模拟场景或作为安全功能的一部分。

## How to: 如何操作？
```Go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

func main() {
    // 种子初始化，以保证随机性
    rand.Seed(time.Now().UnixNano())
    
    // 生成随机数
    randomNumber := rand.Intn(100) // 生成 0 到 99 之间的随机数
    fmt.Println(randomNumber)
}
```
输出可以是：`42`（记住，这是个随机数，每次运行输出可能不同。）

## Deep Dive 深入挖掘
随机数生成起源于古典概率论。Go的`math/rand`包提供伪随机数，因它基于算法，真正随机存在于`crypto/rand`包。两者区别在于：`math/rand`适用于模拟和非安全性要求场合，而`crypto/rand`适用于加密应用。

## See Also 参考链接
- Go官方文档中的`math/rand`包: [https://golang.org/pkg/math/rand/](https://golang.org/pkg/math/rand/)
- Go官方文档中的`crypto/rand`包: [https://golang.org/pkg/crypto/rand/](https://golang.org/pkg/crypto/rand/)
- 关于伪随机数生成器的维基百科页面: [https://en.wikipedia.org/wiki/Pseudorandom_number_generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)