---
title:                "生成随机数"
html_title:           "Clojure: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# 随机数的生成与Clojure编程

## 什么是随机数？为何程序员要这么做？
随机数生成是指从一组数字中随机选择数字的过程。在编程中，我们经常需要使用随机数来模拟真实世界的随机事件或者为程序添加一些元素。这样可以使程序更具有多样性和随机性，从而提升用户体验。

## 如何使用Clojure来生成随机数？
Clojure内置了许多方法来生成随机数，最常用的方法是使用`rand`函数和`rand-int`函数。以下是一个例子：

```Clojure
; 生成一个在0至1之间的随机数
(rand)
; 生成一个从0至10之间的随机整数
(rand-int 10)
```

运行以上代码，得到的结果可能是0.34678和7，每次运行结果都会不同。

## 深入了解
随机数的生成在程序设计中有着重要的作用。在历史上，人们曾经使用物理装置来生成随机数，如骰子和发牌机。但随着计算机的发展，人们发现使用伪随机数更加方便和高效。除了Clojure的内置方法，也有其他程序库可以生成随机数，如Java的`java.util.Random`类和Python的`random`模块。在实际应用中，我们也需要注意随机数的种子(seed)的选择，它会影响随机数生成的结果。

## 查看更多
Clojure官方文档：https://clojure.org/reference/random <br/>
Java Util Random类文档：https://docs.oracle.com/javase/8/docs/api/java/util/Random.html <br/>
Python Random模块文档：https://docs.python.org/3.8/library/random.html