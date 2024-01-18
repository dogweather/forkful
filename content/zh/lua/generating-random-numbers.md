---
title:                "生成随机数"
html_title:           "Lua: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 什么是随机数？为什么程序员要用它？
随机数是指没有规律、没有可预测性的数字。程序员经常使用随机数来模拟偶然性、增加算法的复杂性或者生成加密密钥。

## 如何生成随机数：
```Lua
-- 生成0~1之间的随机小数
math.random()

-- 生成1~100之间的整数
math.random(1, 100)

-- 生成布尔值（true或者false）
math.random() < 0.5

-- 设置随机数种子，使得每次运行程序生成的随机数都不同
math.randomseed(os.time())
```

## 深入了解：
1. 随机数的概念最早由英国数学家高斯提出，用来解决概率论和统计学中的问题。

2. 除了Lua的math库，程序员还可以使用其他库或者算法来生成随机数，比如Linear Congruential Generator（线性同余发生器）和Mersenne Twister（梅森旋转算法）等。

3. 生成随机数的实现原理主要是利用计算机的伪随机数生成器，它通过一个初始值（种子）来生成一个序列的数字，看起来是随机的，但是实际上是按照特定的算法计算得出的。

## 参考链接：
- [Precalc](https://precalc.net/about.php)
- [The Mersenne Twister Home Page](http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html)
- [Linear Congruential Generator](https://en.wikipedia.org/wiki/Linear_congruential_generator)
- [Pseudo Random Number Generators](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)