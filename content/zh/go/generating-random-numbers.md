---
title:    "Go: 产生随机数"
keywords: ["Go"]
---

{{< edit_this_page >}}

为什么：产生随机数是编程中一个常用的技巧，能够让程序变得更加有趣和多样化。无论是给用户展示不同的内容，还是用于游戏、密码生成等领域，随机数都扮演着重要的角色。

如何做到：在Go语言中，产生随机数十分简单。首先，需要导入 `math/rand` 和 `time` 库。然后，使用 `rand.Seed(time.Now().UnixNano())` 来设定随机种子。最后，通过 `rand.Intn()` 函数来产生想要的随机数范围。下面是一个示例代码：

```Go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

func main() {
	// 设置随机种子
	rand.Seed(time.Now().UnixNano())

	// 产生一个0-9的随机数
	randomNum := rand.Intn(10)

	fmt.Println("随机数为：", randomNum)
}
```

运行结果可能为：

```
随机数为： 5
```

深入了解：在生成随机数的过程中，随机种子是十分重要的。它决定了每次运行程序时所产生的随机数序列。如果不设置随机种子，那么每次运行程序都会得到同样的随机数。另外，要注意在多线程环境下，需要使用 `rand.NewSource()` 来设置随机种子，以避免出现竞争条件。此外，Go语言还提供了更多的随机数函数，如 `rand.Float64()`、`rand.Shuffle()` 等，大家可以根据需要进行使用。

查看也可以：想要深入了解随机数的生成过程，可以查看官方文档《生成随机数》（https://golang.org/pkg/math/rand/）。另外，还可以参考《Go语言圣经》一书中关于随机数的介绍（https://books.studygolang.com/gopl-zh/ch6/ch6-05.html）。