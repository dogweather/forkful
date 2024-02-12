---
title:                "生成随机数"
aliases:
- /zh/ruby/generating-random-numbers.md
date:                  2024-01-27T20:35:00.754987-07:00
model:                 gpt-4-0125-preview
simple_title:         "生成随机数"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

在Ruby中生成随机数涉及创建逻辑上无法预测的数字，这对于模拟、加密和游戏等场景至关重要。程序员使用随机性来增加不可预测性或模拟现实生活中的变化性。

## 如何操作:

Ruby通过`Random`类提供了几种生成随机数的方法。

### 基本随机数

生成一个基本随机数：

```Ruby
puts rand(10) # 生成一个0到9之间的随机数
```

### 指定范围的随机数

生成特定范围内的随机数：

```Ruby
puts rand(1..10) # 生成一个1到10之间的随机数
```

### 使用Random类

如果要创建一个可重复的随机数序列，可以使用带有种子的`Random`类。

```Ruby
random_generator = Random.new(1234)
puts random_generator.rand(100) # 生成一个可预测的“随机”数
```

### 生成随机数组元素

从数组中选择一个随机元素：

```Ruby
colors = ["red", "blue", "green", "yellow"]
puts colors.sample # 从数组中随机选择一个元素
```

### 示例输出:

由于其随机性质，以上每个代码片段运行时将产生不同的输出。例如，`rand(10)`可能输出`7`，而`colors.sample`可能输出`"green"`。

## 深入探讨

在计算机科学中生成随机数的概念是悖论的，因为计算机遵循确定性指令。早期方法严重依赖外部输入来实现不可预测性。Ruby的随机性建立在梅森旋转算法上，这是一种伪随机数生成器，以其巨大的周期和均匀分布而闻名，非常适合需要高质量随机性的应用。

虽然Ruby的内置方法在大多数需求上表现良好，但对于所有加密目的来说，可能还不够，因为伪随机数的可预测性可能是一个漏洞。对于加密安全，Ruby开发者可能会探索像`OpenSSL::Random`这样的库，它们旨在产生加密安全的随机数，为敏感应用确保更高的不可预测性。
