---
title:                "Go: 生成随机数"
programming_language: "Go"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么要使用随机数

在编程中，随机数是一种非常常用的概念。它们可以为我们提供随机性，使我们的程序更加有趣和灵活。生成随机数也是学习编程语言的一个重要步骤。

## 如何生成随机数

生成随机数的方法因编程语言而异，但在Go语言中，我们可以使用 `rand` 包的 `Intn()` 函数来生成一个指定范围内的随机整数。

```
import (
    "fmt"
    "math/rand"
)

func main() {
    // 生成0~9之间的随机整数
    fmt.Println(rand.Intn(10))
}
```

输出结果可能为 `6`、`2`、`9` 等不同的数字，这取决于程序每次运行的时机。

除了 `Intn()` 函数，Go语言中还有其他用于生成随机数的相关函数，比如 `Perm()` 函数可以生成随机的排列序列，`Float32()` 和 `Float64()` 函数可以生成随机的浮点数。

## 深入了解随机数的实现原理

在计算机中，真正的随机数是无法实现的，并且伪随机数（pseudo-random）的生成方法也有其局限性。因此，生成的随机数并不是完全随机，而只是具有一定的规律性。在Go语言中，随机数的种子（seed）通常是程序运行的时间，每次程序运行的时候，种子都会不同，从而保证了每次生成的随机数也会不同。

如果需要更高质量的随机数，可以使用 `crypto/rand` 包来生成加密级别的随机数。

# 查看更多资料

1. [Go语言官方文档 - rand包](https://golang.org/pkg/math/rand/)
2. [菜鸟教程 - Go语言随机数](https://www.runoob.com/go/go-random-number.html)
3. [Stack Overflow - How to generate random numbers in Go](https://stackoverflow.com/questions/12321133/generating-random-numbers-in-go) 
4. [Go语言圣经 - 随机数](https://books.studygolang.com/gopl-zh/ch4/ch4-06.html)
5. [知乎 - 计算机中的伪随机数是如何产生的？](https://www.zhihu.com/question/22231431)