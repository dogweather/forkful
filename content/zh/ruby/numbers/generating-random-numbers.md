---
aliases:
- /zh/ruby/generating-random-numbers/
date: 2024-01-27 20:35:00.754987-07:00
description: "\u5728Ruby\u4E2D\u751F\u6210\u968F\u673A\u6570\u6D89\u53CA\u521B\u5EFA\
  \u903B\u8F91\u4E0A\u65E0\u6CD5\u9884\u6D4B\u7684\u6570\u5B57\uFF0C\u8FD9\u5BF9\u4E8E\
  \u6A21\u62DF\u3001\u52A0\u5BC6\u548C\u6E38\u620F\u7B49\u573A\u666F\u81F3\u5173\u91CD\
  \u8981\u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u968F\u673A\u6027\u6765\u589E\u52A0\u4E0D\
  \u53EF\u9884\u6D4B\u6027\u6216\u6A21\u62DF\u73B0\u5B9E\u751F\u6D3B\u4E2D\u7684\u53D8\
  \u5316\u6027\u3002"
lastmod: 2024-02-18 23:08:59.595478
model: gpt-4-0125-preview
summary: "\u5728Ruby\u4E2D\u751F\u6210\u968F\u673A\u6570\u6D89\u53CA\u521B\u5EFA\u903B\
  \u8F91\u4E0A\u65E0\u6CD5\u9884\u6D4B\u7684\u6570\u5B57\uFF0C\u8FD9\u5BF9\u4E8E\u6A21\
  \u62DF\u3001\u52A0\u5BC6\u548C\u6E38\u620F\u7B49\u573A\u666F\u81F3\u5173\u91CD\u8981\
  \u3002\u7A0B\u5E8F\u5458\u4F7F\u7528\u968F\u673A\u6027\u6765\u589E\u52A0\u4E0D\u53EF\
  \u9884\u6D4B\u6027\u6216\u6A21\u62DF\u73B0\u5B9E\u751F\u6D3B\u4E2D\u7684\u53D8\u5316\
  \u6027\u3002"
title: "\u751F\u6210\u968F\u673A\u6570"
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
