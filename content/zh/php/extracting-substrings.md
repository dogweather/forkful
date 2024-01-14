---
title:    "PHP: 提取子字符串"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/extracting-substrings.md"
---

{{< edit_this_page >}}

# 为什么要提取子字符串？

在编写 PHP 程序时经常会遇到需要从一个字符串中提取一部分内容的情况。例如，如果要从一个长的 URL 中提取出网域名称，或者从一个字符串中提取出特定的日期格式，这时就需要用到提取子字符串的技巧。因此，了解如何提取子字符串是非常有用的，可以帮助我们处理字符串数据更高效、更准确。

## 如何做？

为了提取子字符串，我们可以使用 PHP 中内置的 `substr()` 函数。这个函数接受三个参数，分别是原始字符串、起始位置和需要提取的长度。例如，如果我们有一个字符串 `$str = "Hello World"`，想要提取出 "World" 这个子串，可以使用以下方法：

```PHP
<?php
$str = "Hello World";
$substr = substr($str, 6, 5);
echo $substr; // 输出 "World"
```

除了使用固定的起始位置和长度，我们也可以结合使用其他字符串函数来动态地确定需要提取的位置和长度。例如，我们可以使用 `strpos()` 函数来查找子串的起始位置，然后再结合 `strlen()` 函数来确定子串的长度。下面是一个提取 URL 中域名的例子：

```PHP
<?php
$url = "https://www.example.com/posts/123";
$start = strpos($url, "www.") + strlen("www."); // 包含 "www."
$end = strpos($url, "/", $start); // 不包含 "/"
$domain = substr($url, $start, $end - $start);
echo $domain; // 输出 "example.com"
```

## 深入了解

除了基本的 `substr()` 函数外，PHP 还提供了一些其他的字符串函数来帮助我们更灵活地提取子字符串。例如，如果需要提取多个子串，可以使用 `explode()` 函数来将字符串按照指定的分隔符拆分成数组。如果需要移除字符串中的特定部分，可以使用 `str_replace()` 函数来替换子串为空字符串。这些函数都可以在 PHP 官方文档中找到更详细的介绍和示例。

## 参考链接

- [PHP 官方文档 - 字符串函数](https://www.php.net/manual/zh/ref.strings.php)
- [w3cschool - PHP 字符串函数](https://www.w3cschool.cn/php/php-string-functions.html)
- [菜鸟教程 - PHP 字符串处理函数](https://www.runoob.com/php/php-string-functions.html)

# 请参阅

- [PHP 官方文档 - 字符串函数](https://www.php.net/manual/zh/ref.strings.php)
- [w3cschool - PHP 字符串函数](https://www.w3cschool.cn/php/php-string-functions.html)
- [菜鸟教程 - PHP 字符串处理函数](https://www.runoob.com/php/php-string-functions.html)