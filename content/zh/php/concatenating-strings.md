---
title:                "连接字符串"
html_title:           "PHP: 连接字符串"
simple_title:         "连接字符串"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/concatenating-strings.md"
---

{{< edit_this_page >}}

# 为什么

字符串的拼接经常在 PHP 编程中使用。它允许将不同的字符串组合成一个更长的字符串，从而提供更灵活的方式来操作文本数据。拼接字符串的主要目的是为了创建更复杂的表达式或者动态生成文本信息。

# 如何做

使用 PHP 的 "." 运算符来拼接字符串。下面是一个示例代码，它将两个字符串拼接并输出结果：

```PHP
$string1 = 'Hello';
$string2 = 'world';
echo $string1 . ' ' . $string2; // 输出 'Hello world'
```

另一种方式是使用字符串插值，它使用双引号将变量直接嵌入到字符串中。例如：

```PHP
$name = 'John';
echo "Hello, $name"; // 输出 'Hello, John'
```

如果需要拼接多个字符串，可以使用多个 "." 运算符来连接它们。例如：

```PHP
$string1 = 'Hello';
$string2 = 'world';
$string3 = '!';
echo $string1 . ' ' . $string2 . $string3; // 输出 'Hello world!'
```

# 深入了解

在 PHP 中，字符串拼接有多种方法，其中最常用的是使用 "." 运算符。但是，也可以使用 `sprintf()` 函数来格式化字符串并拼接它们。

另外，当需要拼接大量字符串时，推荐使用 `implode()` 函数，它接受一个分隔符和一个数组作为参数，并将数组中的所有元素用分隔符拼接成一个字符串。

最后，PHP 7.4 引入了 `str_contains()` 函数，可以方便地检查一个字符串是否包含另一个字符串，从而避免手动拼接字符串来比较。

# 参考链接

- PHP 官方文档：https://www.php.net/manual/en/language.operators.string.php
- 通过例子学习 PHP 字符串插值：https://www.geeksforgeeks.org/learning-strings-in-php-with-examples-set-2
- 关于 `str_contains()` 函数的详细解释：https://www.php.net/manual/en/function.str-contains.php