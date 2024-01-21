---
title:                "生成随机数"
date:                  2024-01-20T17:49:51.366800-07:00
model:                 gpt-4-1106-preview
simple_title:         "生成随机数"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)
随机数生成是计算机程序创建不可预测数字的过程。程序员用它们来增加安全性、模拟事件或增添游戏的真实感。

## How to: (如何操作：)
```PHP
<?php
// 生成一个随机整数
$randomInt = random_int(1, 100); 
echo $randomInt;

// 生成一个更好的随机字节序列
$randomBytes = random_bytes(5);
echo bin2hex($randomBytes);
?>
```
样本输出：
```
42
e3b7a196eb
```

## Deep Dive (深入了解)
在PHP历史中，`rand()` 与 `mt_rand()` 是常用来生成随机数的函数。但在PHP 7中，引入了 `random_int()` 和 `random_bytes()` 提供更好的随机性和安全性。`random_int()` 适合需要特定范围的整数的情况，而 `random_bytes()` 则可以生成一个随机字节序列，用于加密等场合。

历史长河中，计算机的随机数生成一直面临真随机与伪随机的挑战。真随机是不可预测的，通常用物理现象（如电子噪音）来实现；伪随机是算法生成，显示随机但实际上有确定性。为了安全重要的应用，真随机通常更受欢迎。

有时，你可能需要经常生成随机数，考虑用 `srand()` 来播种你的 `rand()` 或 `mt_rand()`—但要记住，对于PHP 7或更新版本，建议使用 `random_int()` 和 `random_bytes()`。

## See Also (延伸阅读)
- [PHP Manual on random_int](https://www.php.net/manual/en/function.random-int.php)
- [PHP Manual on random_bytes](https://www.php.net/manual/en/function.random-bytes.php)
- [OpenSSL for PHP](https://www.php.net/manual/en/book.openssl.php) - 如果你想了解更深入的加密相关随机数生成方法。