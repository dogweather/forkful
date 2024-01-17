---
title:                "将字符串转换为大写"
html_title:           "PHP: 将字符串转换为大写"
simple_title:         "将字符串转换为大写"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 什么是字符串大写及为什么程序员要做
字符串大写是将字符串中的所有字符转换为大写字母的过程。程序员经常这样做是为了标准化字符串的大小写，使得比较和处理数据更容易。

## 如何实现
下面是一个在PHP中将字符串大写的例子：
```PHP
$str = "hello world";
echo strtoupper($str); // 输出：HELLO WORLD
```

## 深入了解
- 历史背景：在早期的计算机中，大部分字符只有大写形式，因此将字符串转换为大写可以帮助它们在不同平台上的兼容性。
- 其他方法：除了使用内置函数strtoupper()，也可以使用循环和ASCII码来实现字符串大写。
- 实现细节：在大多数编程语言中，字符串是不可变的，因此字符串大写是通过创建一个新的字符串来实现的。

## 参考链接
- [PHP手册 - strtolower()](https://www.php.net/manual/en/function.strtoupper.php)
- [ASCII码表](https://www.ascii-code.com/)