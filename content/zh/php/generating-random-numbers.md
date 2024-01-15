---
title:                "生成随机数"
html_title:           "PHP: 生成随机数"
simple_title:         "生成随机数"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 为什么

随机数在编程中扮演着重要的角色，它可以被用来创建各种各样的游戏、加密算法、测试数据，以及其他需要随机性的应用。因此，学习如何生成随机数将帮助你提升编程技能，以及应用它们到不同的项目中。

## 如何

生成随机数的最基本的方法是使用 `rand()` 函数。它会在给定的两个整数范围内返回一个随机数。例如，要生成1到10之间的随机数，可以使用以下代码：

```PHP
$randomNumber = rand(1, 10);
echo $randomNumber; //可能的输出：6，2，9，3，7，10，1，8，5，4
```

如果需要更大范围的随机数，也可以使用 `mt_rand()` 函数。它使用更好的随机算法，可以生成更大的随机数。以下是一个示例：

```PHP
$bigRandomNumber = mt_rand(1000, 100000);
echo $bigRandomNumber; //可能的输出：23476，82270，41893，52408，100372
```

除了整数，PHP还提供了生成随机字符串的函数 `str_shuffle()`。它可以随机打乱一个字符串的顺序。以下是一个示例：

```PHP
$randomString = str_shuffle('abcdefg');
echo $randomString; //可能的输出：ecgdafb，fabgced，dcebfga，cbegadf
```

## 深入了解

要生成更真实的随机数，可以使用 `random_bytes()` 函数。它会使用系统的随机数生成器来产生更加随机的数字。以下是一个示例：

```PHP
$randomBytes = random_bytes(4);
echo bin2hex($randomBytes); //可能的输出：5a2c9f38，81da99fe，f3c7b649，b8a2e3ee
```

如果需要生成更加安全的随机数，可以使用 `openssl_random_pseudo_bytes()` 函数。它会使用 OpenSSL 库中更加强大的随机数生成器，产生更加安全的随机数。以下是一个示例：

```PHP
$secureRandomBytes = openssl_random_pseudo_bytes(8);
echo bin2hex($secureRandomBytes); //可能的输出：7609e6d00690174c，00650b878149094f，7e70f61c3ae148a1，1b169e361b1d64d0
```

## 查看更多

- [PHP官方文档：生成随机数](https://www.php.net/manual/en/function.rand.php)
- [PHP官方文档：生成随机字符串](https://www.php.net/manual/en/function.str-shuffle.php)
- [PHP官方文档：生成随机字节](https://www.php.net/manual/en/function.random-bytes.php)
- [PHP官方文档：产生更加安全的随机数](https://www.php.net/manual/en/function.openssl-random-pseudo-bytes.php)