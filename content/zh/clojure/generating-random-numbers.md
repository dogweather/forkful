---
title:                "Clojure: 生成随机数"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

### 为什么：生成随机数的重要性
生成随机数在编程中起着重要作用，可以帮助我们模拟实际情况，测试算法，以及增加程序的复杂度。

### 如何生成随机数
在Clojure中，我们可以使用内置函数`rand`来生成随机数。接受一个参数作为随机数的上限，返回一个0到上限之间的随机数。例如，我们可以使用以下代码来生成一个0到10之间的随机数：
```Clojure
(rand 10)
```
运行结果可能如下：
```
5.7745985435835
```
另外，我们也可以使用`do`表达式来生成多个随机数。`do`表达式可以将多条代码组合在一起，在这个例子中，我们将生成两个0到10之间的随机数：
```Clojure
(do
  (rand 10)
  (rand 10))
```
运行结果可能如下：
```
9.2252402383943
6.7632631975952
```
如果我们想要返回一个整数，我们可以使用`int`函数来将随机数转换为整数。例如，我们可以使用以下代码来生成一个0到10之间的整数：
```Clojure
(int (rand 10))
```
运行结果可能如下：
```
8
```
### 深入了解生成随机数
Clojure中的随机数生成是基于Java中的随机数生成器。这意味着如果我们想要生成特定范围的随机数，我们可以使用Java中的`Random`类。另外，我们也可以使用`seed`函数来设置随机数生成器的种子。

### 另请参阅
- [Clojure官方文档](https://clojure.org/)
- [Java随机数生成器文档](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/Random.html)
- [如何使用Clojure中的随机数生成器](https://www.baeldung.com/java-random-list-element)