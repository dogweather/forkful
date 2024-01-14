---
title:                "Ruby: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

随机数是计算机科学中一个有趣且重要的概念。当我们需要在程序中随机地生成数据时，就需要用到随机数。随机数也可以用来模拟不确定的情况，以测试我们的程序的健壮性。在Ruby语言中，有多种方法可以生成随机数，让我们一起来学习如何做到吧！

## 如何做到

在Ruby中生成随机数非常简单。让我们先来了解一下主要的方法：

```Ruby
# 生成0到1之间的随机浮点数
puts rand

# 生成0到100之间的随机整数
puts rand(100)

# 生成指定范围内的随机整数
puts rand(20..30)

# 生成随机布尔值
puts rand() > 0.5
```

输出：

```Ruby
0.9587269
29
27
true
```

通过调用`rand()`方法，我们可以生成一个位于0到1之间的随机浮点数。如果我们需要生成指定范围内的随机整数，我们可以通过在`rand()`方法中传入一个数字来实现。我们还可以使用`..`或`...`来定义一个范围。如果我们需要生成随机的布尔值，我们可以通过比较`rand()`产生的随机数是否大于0.5来实现。

## 深入探讨

在使用随机数时，我们需要注意到它们并不是真正的“随机”。它们实际上是通过一个称为“伪随机数生成器”(PRNG)的算法来产生的。这些算法会根据某个种子值生成一系列看似随机的数字，但这些数字是可以预测的。因此，在编写程序时，我们需要注意如何设置和使用随机数的种子值。

另外，Ruby还提供了一个`SecureRandom`模块，它使用密码学安全算法来生成真正随机的数值。如果我们需要生成安全性更高的随机数，可以考虑使用这个模块。

## 参考链接

- Ruby文档 - [Random Class](https://ruby-doc.org/core-2.7.0/Random.html)
- Ruby文档 - [Kernel Module](https://ruby-doc.org/core-2.7.0/Kernel.html#method-i-rand)
- Ruby文档 - [SecureRandom Module](https://ruby-doc.org/stdlib-2.7.0/libdoc/securerandom/rdoc/SecureRandom.html)