---
title:    "Elixir: 生成随机数"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## 为什么

随机数生成是电脑科学中一个普遍且有用的概念。通过使用随机数，我们可以开发各种各样有趣的应用，比如游戏、密码生成、数据分析等。在Elixir编程中，我们可以轻松地生成随机数来满足各种需求。

## 如何操作

在Elixir中，我们可以使用```Enum.random/1```函数来生成随机数。这个函数接受一个列表作为参数，并从列表中随机选择一个元素。下面是一个简单的例子，展示如何使用```Enum.random/1```来生成一个1到10之间的随机数。

```Elixir
n = Enum.random(1..10)
IO.puts "随机数为: #{n}"
```

运行这段代码，我们会得到类似下面的输出：

```
随机数为: 7
```

除了使用范围（range）来生成随机数，我们也可以使用其他数据结构，比如列表、map等。下面是一个使用列表作为参数的例子：

```Elixir
fruits = ["苹果", "橘子", "香蕉", "草莓"]
random_fruit = Enum.random(fruits)
IO.puts "今天应该吃的水果是: #{random_fruit}"
```

运行这段代码，我们会得到类似下面的输出：

```
今天应该吃的水果是: 香蕉
```

## 深入了解

Elixir中的随机数生成是基于Erlang的```random```模块实现的。这个模块使用了一种叫做“伪随机数生成器”（PRNG）的算法来产生随机数。PRNG会通过一个初始种子（seed）来生成随机数，从而保证每次随机数的产生是可复制的。同样的种子会产生同样的随机数序列。我们可以通过设置种子来控制随机数的产生，从而达到我们想要的结果。

如果我们想要产生完全随机的数，我们可以不设置种子，这样每次运行程序就会产生不同的随机数序列。但是如果我们想要复现之前的结果，比如游戏中的随机事件，我们就需要手动设置种子。

## 参考阅读

- [Erlang Random模块文档](http://erlang.org/doc/man/random.html)
- [Elixir Enum模块文档](https://hexdocs.pm/elixir/Enum.html)
- [Elixir Enum.random/1函数文档](https://hexdocs.pm/elixir/Enum.html#random/1)
- [使用Elixir生成随机数的一些技巧](https://pusher.com/tutorials/random-number-generation-elixir)

## 参见

如果你对Elixir的其他特性感兴趣，可以阅读这些相关的文章：

- [如何在Elixir中操作字符串](https://example.com)
- [Elixir中的并发编程指南](https://example.com)
- [使用Ecto来管理数据库](https://example.com)