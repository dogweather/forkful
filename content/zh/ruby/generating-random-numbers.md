---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么？
生成随机数是一种编程方法，用于产生无可预测的数值。由于其不确定性，程序员用它来执行涵盖安全性、数据测试、游戏等多种应用的任务。

## 如何实现：
在 Ruby 中，生成随机数很简单。使用内置的 `rand` 方法和 `Random` 类，可以方便的生成随机数。如下示例：
```Ruby
# 使用`rand`生成0到1之间的浮点数
random_float = rand 
puts random_float
# 输出: 0.5872154721384615

# 使用`rand`生成1到10之间的整数
random_number = rand(1..10) 
puts random_number
# 输出: 5

# 使用`Random`类生成特定范围的随机数
random = Random.new
random_number = random.rand(1..100)
puts random_number
# 输出: 42
```
## 深度解析：
生成随机数在计算机世界中有着悠久的历史，和编程一样早。在 Ruby 中，`rand` 和 `Random` 提供了强大且灵活的接口生成随机数。
对于选择 `rand` 和 `Random` 的考量取决于你需要的随机数类型和范围。`rand` 适合生成0到1之间的浮点数或小范围的整数。`Random` 类适合生成更大范围或更特定类型的随机数。
此外，Ruby 使用 Mersenne Twister 算法生成随机数，这是一种具有 2^19937 − 1 期的快速伪随机数生成器，深受开发者喜爱。

## 延伸阅读：
* Ruby 官方文档对 `rand` 方法和 `Random` 类有着详细的介绍: 
[rand](https://ruby-doc.org/core-2.4.1/Kernel.html#method-i-rand) 和 [Random](https://ruby-doc.org/core-2.5.1/Random.html) 
* Mersenne Twister 算法的详细解析: [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister) 
* 更多随机数生成的应用: 
[Random Number Generation in Ruby](https://www.rubyguides.com/2019/05/ruby-random/)