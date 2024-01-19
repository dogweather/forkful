---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么 & 为什么?

随机数生成是编程中一种方式，可以让你获得不可预知的数字。这在许多应用中都非常有用，比如在游戏中创建不确定性，进行统计抽样，或者在加密算法中生成密钥。

## 怎么做：

在Clojure中，我们可以使用`rand`函数来生成随机数。这是一些示例：

```Clojure
(rand) 
=> 0.8991766455976201

(rand-int 100)
=> 42
```

`rand`函数返回一个大于等于0且小于1的随机浮点数。`rand-int`函数将返回一个在给定范围内的随机整数。

## 深入探讨：

随机数生成在历史上一直是一个重要的技术问题。最早的随机数生成器只是简单地从一张表格上选择数字，但现在我们可以使用复杂的数学算法来生成随机数。

Clojure的`rand`和`rand-int`函数使用Java的`java.util.Random`类来生成随机数。这是一个基于线性同余生成器的伪随机数生成器。

之所以说“伪”随机数，是因为在理论上，如果你了解生成器的内部状态，你可以预测它将来会生成的数字。不过，对于大多数应用来说，这种预测是非常困难的。

也有其他的随机数生成算法，比如梅森旋转算法（Mersenne Twister）和WELL算法。这些算法有各种优点和缺点，取决于你的应用需求。

## 另请参阅：

- [Clojure官方文档：随机数](https://clojuredocs.org/clojure.core/rand)
- [Java中的随机数生成](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Wikipedia：随机数生成](https://zh.wikipedia.org/wiki/%E9%9A%8F%E6%9C%BA%E6%95%B0%E7%94%9F%E6%88%90%E5%99%A8)