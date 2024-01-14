---
title:                "Clojure: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 为什么要生成随机数？

在编程的世界里，随机数是一个非常重要的概念。它可以被用来模拟现实世界中的随机事件，帮助我们创建可重复的代码，甚至可以构建随机性的游戏。无论是什么原因，生成随机数都是非常有用的技巧，让我们来看看如何在Clojure中实现它。

## 如何生成随机数？

在Clojure中，我们可以使用`(rand)`函数来生成一个0到1之间的随机数。如果想要生成一个指定范围内的随机整数，则可以使用`(rand-int n)`函数，其中 n 为范围。下面是一个例子：

```Clojure
(rand) ; => 0.7480682118451838

(rand-int 10) ; => 7
```

如果我们想要生成一组随机数，可以使用`for`函数和`range`函数来实现。下面的代码将生成10个随机数，并将它们存储在一个列表中。

```Clojure
(for [x (range 10)]
  (rand)) ; => (0.40134497126876535 0.05613902643835942 0.9254182569730625 0.5003199372819021 0.6432034908278608 0.5797597669106045 0.930961409594252 0.7520485869047621 0.8341718954541113 0.5105994259117965)
```

## 深入研究生成随机数

在计算机科学中，真正的随机数是不存在的，因为计算机只能按照指令执行，无法真正地产生随机数。因此，计算机科学家们使用伪随机数来模拟真正的随机性。伪随机数是通过一个特定的算法来生成的，这个算法需要一个种子值来作为输入。种子值是生成随机数的起点，相同的种子值会产生相同的随机数序列。在Clojure中，我们可以通过`(random-seed n)`函数来设置种子值。

除了上面提到的函数，Clojure中还有一些其他的函数来帮助我们生成随机数。例如，`(shuffle coll)`函数可以随机打乱一个集合元素的顺序，`(<)`, `(<=)`, `(>)`, `(>=)`函数可以帮助我们判断两个值的大小关系，`(rand-nth coll)`函数可以随机选择一个集合中的元素。

# 另请参阅

- [Clojure官方文档 - 随机数](https://clojure.org/reference/java_interop#random-numbers)
- [Clojure维基百科 - 随机数发生器](https://en.wikipedia.org/wiki/Random_number_generation_in_Clojure)
- [Clojure Cookbook - 生成随机数](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/08_data/8-12_random_numbers.md)