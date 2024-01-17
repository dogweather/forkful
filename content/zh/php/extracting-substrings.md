---
title:                "提取子串"
html_title:           "PHP: 提取子串"
simple_title:         "提取子串"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## 什么是提取子字符串？为什么程序员要这么做？

提取子字符串是指从一个字符串中截取一个子字符串。程序员通常会这样做是因为在处理文本和数据时，我们经常需要从字符串中提取特定的信息，例如从一个邮箱地址中获取用户名，或者从一个网址中提取域名。

## 如何提取子字符串：

提取子字符串可以通过使用 ```substr() ``` 函数来实现。下面是一个示例代码，它会从一个字符串中提取前五个字符，并打印输出结果：

```
<?php
$str = "Hello World";
echo substr($str, 0, 5); // output: Hello
?>
```

可以看到我们传递给 ```substr() ``` 函数的参数分别是字符串本身，要提取子字符串的起始位置，以及要提取的长度。我们也可以以负数作为起始位置，从字符串末尾开始提取子字符串。例如，```substr($str, -5) ``` 将从字符串的倒数第五个字符开始提取子字符串。

## 深入了解提取子字符串的历史背景、替代方案和实现细节：

提取子字符串是一种在计算机编程中经常使用的技术。它在早期程序设计语言中就有出现，如BASIC语言中的```LEFT$ ``` 和 ```RIGHT$ ``` 函数。而在PHP之外，其他替代方案也包括使用正则表达式或字符串处理工具来提取子字符串。在PHP中，我们也可以使用 ```explode() ``` 函数将字符串按照某个分隔符拆分，并提取其中的某一部分。另外，对于较长的字符串，提取子字符串的性能会受到影响，因此在处理大量数据时，建议使用其他更高效的方法来提取子字符串。

## 参考资料：

- PHP官方文档：[substr()函数](https://www.php.net/manual/zh/function.substr.php)
- PHP官方文档：[explode()函数](https://www.php.net/manual/zh/function.explode.php)
- PHP官方文档：[正则表达式](https://www.php.net/manual/zh/book.pcre.php)