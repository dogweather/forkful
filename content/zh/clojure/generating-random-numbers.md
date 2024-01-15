---
title:                "生成随机数字"
html_title:           "Clojure: 生成随机数字"
simple_title:         "生成随机数字"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

Clojure是一种功能强大的编程语言，它提供了一种简单有趣的方式来生成随机数。随机数在计算机科学中有许多用途，例如加密、游戏、模拟和测试。您可以使用Clojure生成随机数来解决各种问题，同时也能让编程过程更加有趣。

## 如何使用

```Clojure 
;;生成一个随机数
(def random-number (rand))
;;输出随机数
(println "随机数是：" random-number)
;;生成指定范围内的随机数
(rand-int 10) ;;生成0到9的随机数
```

上面的代码展示了如何在Clojure中生成随机数，并将其打印出来。您可以将这些随机数应用到您的项目中，例如创建一个简单的猜数字游戏：

```Clojure
(def secret-number (rand-int 10)) ;;生成一个0到9的随机数

(defn guess-number []
  (println "猜一个数字：")
  (let [user-input (read-line) ;;读取用户输入
        user-guess (Integer/parseInt user-input)] ;;将输入转换为整数
    (cond
      (> user-guess secret-number) (println "太大了，再试一次！") (guess-number)
      (< user-guess secret-number) (println "太小了，再试一次！") (guess-number)
      :else (println "你猜对了！") ;;用户猜中了，游戏结束
    )
  )
)

(guess-number)
```

每次用户猜错数字时，游戏会提醒他们数字太大或太小，并继续要求输入直到猜中为止。

## 深入探讨

Clojure使用Java的`java.util.Random`类来生成随机数。`rand`和`rand-int`函数分别调用了`Random`类中的`nextDouble`和`nextInt`方法来生成随机数。您还可以使用`rand-nth`函数来从集合中随机选择一个元素。

正如我们所见，Clojure中生成的随机数是伪随机数。它们并不是真正随机的，而是通过使用固定的算法和种子值来生成的。因此，如果使用相同的种子值，每次生成的随机数都将相同。我们可以使用`seed`函数来设置种子值，也可以设置`rand`函数的`boolean`参数来指定是否使用系统时间作为种子值。

## 参考资料

- [Clojure官方网站](https://clojure.org/)
- [Random类的文档](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Clojure Cookbook：随机数](https://clojure-cookbook.org/numeric/random_numbers)

## 另请参阅

- [Clojure在realpython.com的入门教程](https://realpython.com/clojure-introduction/)
- [Clojure编程的7个功能特性](https://dev.to/cyberos/clojure-programming-in-seven-really-great-functions-4pfo)
- [Clojure & Functional Programming：一种简单方法来编写更好的代码](https://towardsdatascience.com/a-simple-way-to-write-better-code-for-clojure-functional-programming-c4a8b6a97dd8)