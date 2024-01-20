---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 是什么以及为什么？

随机数生成是一种编程方法，用于产生不可预测的数字值。编程师通过使用随机数生成来测试性能，创建复杂的动画或增加游戏和应用的元素的难度。

## 如何做：

以下是在Elm中生成随机数的简单例子：
 
```Elm
import Random

main =
    Random.generate identity (Random.int 1 100)
```

在这个例子中，`Random.int 1 100`产生一个介于1和100之间的随机整数。 `Random.generate`函数则将产生的随机数传递给主函数(main)。

输出样例：

```Elm
Result : Int
Result = 42
```
## 深入研究：

历史背景：在先前的编程语言中，生成随机数通常依赖于硬件或操作系统的随机数生成器。而在Elm中，放弃了这个工具包，改用函数式的方式生成随机数字，也就是说，生成的随机数是由其验证过的函数决定的。

替代方案：如果你不像使用Elm随机库，你可以使用外部服务，例如随机化API或者使用JavaScript互操作。

实现