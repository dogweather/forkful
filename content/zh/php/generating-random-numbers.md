---
title:                "生成随机数字"
html_title:           "PHP: 生成随机数字"
simple_title:         "生成随机数字"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 什么是生成随机数？为什么程序员需要它？

生成随机数是指根据一定的规则，计算机可以生成一个随机的数值。程序员经常使用这个功能来增加程序的随机性，使程序更加多样化和有趣。例如，在游戏中生成随机的地图或随机的游戏行动，可以给玩家带来不同的体验。

## 如何生成随机数：

```PHP
// 生成一个范围在1到10的随机整数
$random = rand(1, 10);
echo $random;
// 输出可能为：5、2、8等等

// 生成一个长度为6的随机字符串
$randomStr = bin2hex(openssl_random_pseudo_bytes(3));
echo $randomStr;
// 输出可能为：a5c7d2、1346ef等等
```

## 深入了解：

生成随机数的计算法则可以追溯到19世纪，但直到计算机出现后，才真正成为程序员的工具。除了使用`rand`函数生成随机整数外，还可以使用`mt_rand`、`random_int`或`openssl_random_pseudo_bytes`等函数。此外，也可以使用种子（seed）来控制随机数的生成。例如，利用当前时间作为种子可以确保每次运行程序时生成的随机数不同。

## 链接：

- [PHP官方文档-随机数函数](https://www.php.net/manual/en/ref.math.php)
- [维基百科-随机数生成器](https://en.wikipedia.org/wiki/Random_number_generation)