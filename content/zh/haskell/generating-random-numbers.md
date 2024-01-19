---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么？
生成随机数就是创建没有明显规律、不容预测的数字。程序员利用随机数来进行测试，模拟真实世界情况，或为算法提供随机输入。

## 如何实现：
下面是在Haskell中生成随机数的示例代码和输出结果：

```Haskell
import System.Random

main = do
    g <- newStdGen
    print . take 5 $ randomRs ('a', 'z') g
```
上面的代码将生成一个在'a'和'z'之间的5个随机字符的列表。例如：

```Haskell
"dkmzp"
```

## 深度探索：
在历史上，我们通常使用不完全随机的方法来生成随机数。然而，随着编程语言的发展，我们现在可以使用随机库如Haskell的System.Random来生成真正的随机数。

当然，也有其他的随机数生成方式，例如使用不同种子或算法，或者使用像DevRandom这样的设备来生成随机数。具体使用哪种方法主要取决于我们对随机性、性能和易用性的需求。Haskell中的Standard Generator（StdGen）是一种懒生成（lazy）的方法，它只在需要时才生成新的随机数。

## 另见：
关于生成随机数和Haskell的更多信息，您可以参照以下链接：

1. [Haskell Library - System.Random](http://hackage.haskell.org/package/random-1.1/docs/System-Random.html)
2. [Haskell Wiki - Random Numbers](https://wiki.haskell.org/Random_numbers)
3. [Learn You a Haskell For Great Good - Input and Output](http://learnyouahaskell.com/input-and-output)