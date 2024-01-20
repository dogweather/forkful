---
title:                "提取子字符串"
html_title:           "Arduino: 提取子字符串"
simple_title:         "提取子字符串"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何为 "子字符串" 及其重要性？
子字符串（substring）是字符串中的一部分字符的序列。在编程中，我们往往需要使用到提取子字符串的技能，因为它能有效地处理和操作原始数据，使我们更准确地获取信息。

## 如何实现：
PHP 提供了 `substr` 方法来提取字符串的子字符串。以下是一些例子：

```PHP
<?php
    $string = "Hello, World!";
    
    // 获取字符串中的部分字符
    $substring = substr($string, 7, 5);
    echo $substring;  // 输出： World
?>

```
在此例中，`substr` 方法提取出了 "Hello, World!" 字符串的第7到12个字符，结果为 "World"。

## 深入研究：

早在旧版的 PHP 中，就已经引入了 `substr` 函数。此函数是从 C 语言中借鉴而来的，因为 `substr` 是各种编程语言中很常见的一种实用功能。

虽然 `substr` 是提取子字符串的一个很好的方式，但同时 PHP 也提供了其他一些方法，如 `mb_substr`，它用于处理包含多字节字符（例如 UTF-8 编码的字符串）的字符串。

`substr` 函数的实现在源代码中可以找到，它调用了PHP内部的一些函数和宏。然而，对于 PHP 开发者来说，了解其如何在底层实现并不是必要的；他们只需要知道如何使用它就足够了。

## 更多资源：
1. [PHP官方文档：substr函数](http://php.net/manual/zh/function.substr.php)
2. [PHP官方文档：mb_substr函数](http://php.net/manual/zh/function.mb-substr.php)
3. [PHP：substr VS mb_substr](https://stackoverflow.com/questions/3956900/php-mb-substr-vs-substr)
4. [C语言的子字符串函数：strncpy](https://www.cplusplus.com/reference/cstring/strncpy/)