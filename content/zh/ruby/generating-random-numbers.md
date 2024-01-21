---
title:                "生成随机数"
date:                  2024-01-20T17:49:57.091263-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?（是什么？为什么？）
生成随机数指的是在一定范围内创建不可预测的数字。程序员用它们来增加安全性、支持游戏逻辑，或进行科学模拟。

## How to:（怎么做？）
```ruby
# 基础用法
rand_num = rand(100) # 0到99之间的一个整数
puts rand_num

# 获得一个0.0到1.0之间的浮点数
rand_float = rand
puts rand_float

# 产生一个1到10之间的整数
rand_range = rand(1..10)
puts rand_range

# 用Random类生成
rand_obj = Random.new
rand_obj_num = rand_obj.rand(100)
puts rand_obj_num
```

可能的输出：
```
42
0.453927104897656
7
84
```

## Deep Dive（深入了解）
随机数生成的历史悠久，是密码学、游戏开发、科学计算的重要组成部分。Ruby的`rand`方法使用了梅森旋转算法（Mersenne Twister），算法快速且质量高。除了`rand`和`Random`类，还有`SecureRandom`库，适合需要更强随机性的场合（例如生成加密密钥）。在实现方面，随机数生成器通常基于称为“种子”的起始数值，

## See Also（另请参见）
- [Ruby 官方文档 - Random类](https://ruby-doc.org/core-2.7.1/Random.html)
- [Ruby 官方文档 - Kernel模块#rand方法](https://ruby-doc.org/core-2.7.1/Kernel.html#method-i-rand)
- [维基百科 - 随机数生成](https://zh.wikipedia.org/wiki/%E9%9A%A8%E6%9C%BA%E6%95%B0%E7%94%9F%E6%88%90)
- [维基百科 - 梅森旋转算法](https://zh.wikipedia.org/wiki/%E6%A2%85%E6%A3%AE%E6%97%8B%E8%BD%AC%E7%AE%97%E6%B3%95)