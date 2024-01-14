---
title:    "Go: 生成随机数"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么要生成随机数？

生成随机数在编程中是一项很常见的任务，它可以用来模拟真实世界中的随机事件，例如扑克牌游戏中的发牌、抽奖和随机生成测试数据等。通过使用随机数，我们可以让程序更具有变化性和随机性，从而增加其趣味性和复杂性。

## 如何生成随机数？

首先，我们需要导入Go语言中的math/rand包，它提供了生成伪随机数的功能。然后，我们可以使用rand.Intn(n)函数来生成一个0到n-1之间的随机整数。下面是一个示例代码和输出结果：

```Go
package main

import (
	"fmt"
	"math/rand"
)

func main() {
	// 生成一个0到9之间的随机整数
	num := rand.Intn(10)
	fmt.Println(num)
}
```

输出结果可能为：5、3、9等等，每次运行结果都会不同。

## 深入了解随机数生成

如果我们想要生成更复杂的随机数，可以使用rand.Float64()函数来生成一个0.0到1.0之间的随机浮点数。同时，为了保证每次运行结果的随机性，我们可以通过调用rand.Seed()函数来设定随机数的种子。我们也可以使用crypto/rand包来生成更加安全的随机数。

## 参考链接

- [Go语言官方文档 - math/rand包](https://golang.org/pkg/math/rand/)
- [Go语言官方文档 - crypto/rand包](https://golang.org/pkg/crypto/rand/)
- [Go语言教程 - 生成随机数](https://www.runoob.com/go/go-random-number.html)