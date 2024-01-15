---
title:                "生成随机数"
html_title:           "Ruby: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/ruby/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

有时候，在编程中需要使用随机数。比如，当需要选择一个随机元素时，或者创建一个随机的游戏环境。

## 如何使用

```Ruby
# 生成一个 0-100 之间的随机整数
random_number = rand(0..100) 
# => 42
```

```Ruby
# 从给定的数组中随机选择一个元素
fruits = ["apple", "banana", "orange", "kiwi"]
random_fruit = fruits.sample
# => "orange"
```

```Ruby
# 创建一个随机的多项选择题
questions = ["What is the capital of France?", "What is 2 + 2?", "What color is an orange?"]
random_question = questions.sample
# => "What is 2 + 2?"
```

## 深入了解

Ruby 提供了内置的 `rand` 方法来生成随机数。它可以接收两个参数，第一个参数是范围的起始值，第二个参数是结束值。例如 `rand(0..100)` 可以生成一个 0 到 100 之间的随机整数。

如果需要从给定的数组中随机选择一个元素，可以使用 `sample` 方法。它会随机选择一个元素并返回。除了数组，还可以使用 `sample` 方法来从字符串、哈希表等数据结构中随机选择元素。

## 猜一个数字游戏

为了深入了解随机数生成，我们可以一起来创建一个“猜一个数字”的游戏。首先，我们需要生成一个 1 到 100 之间的随机整数作为目标数字，让用户猜测。每次用户猜测，我们都会给出提示，比如猜测的数字是否在目标数字的左边或右边。直到用户猜对为止，游戏会一直进行。

## 查看也可以

- [Ruby doc - Random class](https://ruby-doc.org/core-2.7.1/Random.html)
- [Ruby Monk - Generating Random Numbers](https://rubymonk.com/learning/books/4-ruby-primer-ascent/chapters/45-more-classes/lessons/112-generating-random-numbers)