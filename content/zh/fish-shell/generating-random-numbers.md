---
title:                "产生随机数"
html_title:           "Fish Shell: 产生随机数"
simple_title:         "产生随机数"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么要使用随机数

在编程中，随机数是一个非常有用的概念。它可以帮助我们创建随机化的数据，并模拟真实世界的情况。比如在游戏开发中，随机数可以用来生成随机的敌人出现位置，使得游戏更具挑战性和多样性。因此，使用随机数可以为我们的程序增加一些乐趣和惊喜因素。

## 如何使用Fish Shell生成随机数

在Fish Shell中，生成随机数非常简单。我们可以使用内置的`math`命令来调用系统的随机数生成器，然后指定一个范围来生成随机数。举个例子，我们想要生成1到100之间的随机整数，可以这样写：
 
```Fish Shell
set random_number (math random --max 100 + 1)
echo $random_number
```

运行结果可能是`69`，每次运行都会得到一个不同的随机数。同样的，我们也可以生成随机小数，比如生成一个0到1之间的随机数，可以这样写：

```Fish Shell
set random_number (math "random(0,1)")
echo $random_number
```

输出可能是`0.7587`，每次运行得到的随机小数也不相同。使用Fish Shell生成随机数非常方便快捷，让我们的程序更具变化性和趣味性。

## 深入了解随机数生成

在计算机中，随机数并不是真正的随机数，而是伪随机数。它们是通过算法来生成的，但也具有一定的随机性。在Fish Shell中，我们使用的是`L'Ecuyer-CMRG`算法来生成随机数，它是一种多维平衡迭代器。我们也可以通过设置种子数来控制随机数的生成，保证每次运行得到的随机数序列相同。

另外，除了`math`命令外，我们也可以使用`seq`命令来生成一系列随机数，更加灵活地控制范围和步长。总的来说，随机数在编程中可以用来模拟真实情况、增加趣味性和难度，也可以用来做密码学等安全性相关的工作。通过使用Fish Shell，我们可以简单地生成随机数，提高程序的效率和有趣性。

## 参考链接

* [Fish Shell官方文档](https://fishshell.com/docs/current/index.html)
* [L'Ecuyer-CMRG算法的介绍](http://www.iro.umontreal.ca/~lecuyer/myftp/papers/rngjava-scix-1.3.pdf)
* [随机数的应用场景](https://www.cnblogs.com/yangxiaolan/p/5779810.html)