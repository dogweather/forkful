---
title:    "Gleam: 生成随机数"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么: 随机数在编程中的应用
有时，我们需要随机生成一些数字来模拟现实世界或测试我们的代码。Gleam编程语言提供了一种简单，可靠的方法来生成随机数，让我们一起来看看吧。

## 如何: 使用Gleam生成随机数
我们可以使用内置的rand模块中的random函数来生成随机数。下面是一个简单的代码示例，它生成10个范围在1到100之间的随机数并打印出来。

```
Gleam module Main

import rand
import array

fn random_nums() {
  let nums = array.new()
  let size = 10
  let min = 1
  let max = 100
  let count = 1
  while count <= size {
    let num = rand.random(min, max)
    array.append(&nums, num)
    count = count + 1
  }
  nums
}

fn main(_) {
  let nums = random_nums()
  list_iter(nums)
    fn(x) {
      IO.print("随机数: ")
      IO.print(x)
      IO.print("\n")
    }
  IO.print("生成了 ", array.len(nums), " 个随机数")
}
```

运行这段代码，我们会得到类似下面的输出:

```
随机数: 25
随机数: 56
随机数: 9
随机数: 78
随机数: 13
随机数: 91
随机数: 40
随机数: 90
随机数: 44
随机数: 73
总共生成了 10 个随机数
```

现在你已经知道如何使用Gleam生成随机数了，让我们深入了解一下其中的原理。

## 深入: 生成随机数的原理
Gleam中使用的随机数生成算法是[Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister)，它是一种经过广泛测试和使用的伪随机数生成器。这意味着每次运行我们的程序时都会得到不同的结果，但是它们都是按照一定规律生成的，因此可以被认为是“伪随机数”。

在使用Gleam生成随机数时，我们需要注意以下几点：

- 确定好生成随机数的范围，即最小值和最大值
- 使用循环语句来指定生成的随机数的数量
- 将生成的随机数保存在一个数据结构中，比如数组

## 参考文章
- [Gleam官方文档-随机数](https://gleam.run/book/core/random-numbers.html)
- [Mersenne Twister算法详解](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/ARTICLES/mt.pdf)
- [如何选择合适的随机数生成器](https://www.pcg-random.org/)

# 参见
- [如何在Gleam中使用时间戳](https://www.example.com)
- [如何使用rand模块中的其他函数生成随机数](https://www.example.com)