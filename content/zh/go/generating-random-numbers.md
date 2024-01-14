---
title:                "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么生成随机数

生成随机数在编程中是一个常见的需求，它可以用于各种应用，比如游戏、密码生成和数据随机化。在Go语言中，有多种方法可以生成随机数，让我们一起来看看吧！

## 如何生成随机数

在Go语言中，我们可以使用math/rand包来生成随机数。首先，我们需要导入这个包：

```Go
import "math/rand"
```

然后，我们可以使用`rand.Intn()`函数来生成一个范围在0到n-1之间的随机整数：

```Go
fmt.Println(rand.Intn(10)) // 输出一个0到9之间的随机整数
```

我们也可以生成一个范围在n到m之间的随机整数：

```Go
fmt.Println(rand.Intn(m-n+1) + n) // 输出一个n到m之间的随机整数
```

如果需要更加精确的随机数，我们可以使用`rand.Float64()`函数来生成一个范围在0.0到1.0之间的随机浮点数：

```Go
fmt.Println(rand.Float64()) // 输出一个0.0到1.0之间的随机浮点数
```

我们也可以通过设置种子数来生成伪随机数，以保证每次运行程序时都能产生相同的随机数序列：

```Go
rand.Seed(seed) // 设置种子数
```

## 深入了解随机数生成

在计算机科学中，真正的随机数是不存在的，因为计算机是按照特定的算法来生成随机数的。因此，我们生成的随机数实际上是伪随机数，只是在短时间内看起来是随机的。

在Go语言中，使用的是伪随机数生成器（Pseudorandom Number Generator, PRNG），它根据设置的种子数来生成随机数序列。如果不设置种子数，默认会使用系统当前时间作为种子数。

另外，我们也可以使用crypto/rand包来生成安全的随机数，它使用更加复杂的随机数生成算法，生成的随机数更为随机和安全。

## 参考链接

- [Go语言官方文档-随机数生成](https://golang.org/pkg/math/rand/)
- [Go语言官方文档-crypto/rand包](https://golang.org/pkg/crypto/rand/)
- [有关随机数的更深层次的解释](https://www.youtube.com/watch?v=SxP30euw3-0)

# 参见

- [Go语言教程-Go语言基础](https://www.runoob.com/go/go-tutorial.html)
- [如何在Go语言中生成随机密码](https://www.digitalocean.com/community/tutorials/how-to-generate-random-passwords-in-go)
- [生成随机数的原理解析](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)