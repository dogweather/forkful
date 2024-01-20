---
title:                "查找字符串的长度"
html_title:           "Javascript: 查找字符串的长度"
simple_title:         "查找字符串的长度"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# PHP 字符串长度求解

## 什么 & 为什么？

在编程中，字符串长度的概念就是计算在一个字符串中字符的数量。为什么要方便的获取字符长度呢？这是因为，对于数据验证，输入限制，或者字符串操作等方面，知道字符串的长度都是非常有帮助的。

## 如何求解：

PHP 中获取字符串长度的标准方式是使用 `strlen()` 函数。例子如下：

```PHP
$str = "Hello, 世界!";
echo strlen($str);
```

输出结果：

```PHP
15
```

注意对于含有非ASCII字符的字符串, `strlen()` 函数计算的可能不是你预期中的结果, 这是因为它在计算时是按照字节来计数的, 比如 "世界" 占用了 6 个字节. 如果要正确的计算出字符数量, 我们则需要使用 `mb_strlen()` 函数.

```PHP
$str = "Hello, 世界!";
echo mb_strlen($str, 'utf8');
```

输出结果：

```PHP
9
```

## 深入探讨

在历史上，由于早期的计算机主要在处理英文字符，因此 `strlen()` 的设计初衷就是以字节为单位的。随着全球化的需求增长, 对支持多语言环境的需求也日益增长, 这就引入了多字节字符集 (比如 UTF-8) 的支持. `mb_strlen()` 方法就是在这样的背景下诞生的。

除了 `strlen()` 或 `mb_strlen()`，你也可以使用其它方式来获取字符串长度，例如 `substr_count($str, "") - 1` 也可达此目的，但是效率上可能不如前者。

## 参考资料

1. [PHP 官方文檔：strlen](https://www.php.net/manual/zh/function.strlen.php)
2. [PHP 官方文檔：mb_strlen](https://www.php.net/manual/zh/function.mb-strlen.php)
3. [Stack Overflow: PHP 字符串长度计算含有中文字符](https://stackoverflow.com/questions/17342504/what-is-the-difference-between-strlen-and-mb-strlen)