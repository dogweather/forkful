---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 什么是随机数生成 & 为什么需要它？

随机数生成是指通过代码来产生一系列看似没有规律的数字。程序员经常需要随机数来模拟真实的世界，例如测试游戏，或者创建密码。通过随机数生成，程序可以产生多样化的结果，从而使程序更加具有灵活性和真实性。

# 如何实现随机数生成？

Go语言提供了内置的rand包，可以轻松生成随机数。下面的示例展示了如何使用该包生成一个1到10之间的随机数：

```Go
rand.Seed(time.Now().UnixNano())
fmt.Println(rand.Intn(10) + 1)
```

该代码首先设置随机数种子，以确保每次运行结果都不同。然后，使用Intn()函数来生成1到10之间的随机数。每次运行该代码，都会得到不同的结果。

# 深入探究

创建随机数的需求可以追溯到早期计算机时代。在那个时候，计算机计算能力有限，随机数可以帮助程序尽可能地模拟真实世界。除了内置的rand包外，还有一些流行的第三方库可以生成更复杂的随机数，如crypto/rand。但是，由于随机数是的本质上是不可预测的，因此需要谨慎使用，避免出现安全漏洞。

# 参考资料

- 官方Go语言文档：https://golang.org/pkg/math/rand/
- 第三方随机数库crypto/rand文档：https://pkg.go.dev/crypto/rand