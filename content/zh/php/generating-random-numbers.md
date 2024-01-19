---
title:                "生成随机数"
html_title:           "Go: 生成随机数"
simple_title:         "生成随机数"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么和为什么?
生成随机数是计算机程序指定范围内创建一个了无规律但确实有唯一值的过程。一些功能，如密码生成，安全哈希，或游戏编程，需要随机数以保证其结果的不可预测性。

## 如何:
使用PHP生成随机数相当简单。这里是一个核心代码示例:

```PHP
<?php
$randomNumber = rand(1, 100);
echo $randomNumber;
?>
```
这个代码片段将为您生成一个介于1和100之间的随机数。您只需简单调用`rand()`函数。

深度探究
生成随机数的历史可以追溯到古希腊时期乃至更早，但在计算机编程中的使用则开始于上世纪五十年代。在PHP中，`rand()`函数是其中的一个基础实现，但这并不是唯一的方法。PHP 7中引入了`random_int()`和`random_bytes()`函数，这两个函数是更安全的随机数生成方法，特别是在涉及密码或加密的情况下。

具体实现上，随机数生成器背后通常使用某种算法（如梅森旋转算法或伪随机数生成器）。这些算法接受一个“种子”值，然后用它来创建一个随机序列。每个随机数生成器都有其特定的优点和缺点，通常取决于其安全性，速度，和随机性。

## 另请参阅:
生成随机数比大家想象的要复杂。要了解更多相关信息，可以参阅以下链接：

1. [PHP手册：random_int](https://www.php.net/manual/en/function.random-int.php)
2. [PHP手册：random_bytes](https://www.php.net/manual/en/function.random-bytes.php)
3. [维基百科：随机数生成器](https://en.wikipedia.org/wiki/Random_number_generation)

在编程实战中反复使用和理解才是掌握这项技能的关键。希望大家在练习中获得亮点。