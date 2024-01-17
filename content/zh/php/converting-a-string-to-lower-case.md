---
title:                "将字符串转换为小写"
html_title:           "PHP: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

#什么是「toLowerCase」？ 
「toLowerCase」是一种在编程中将字符串转换为小写的操作。这通常用于比较字符串的值时，可以忽略大小写，使得比较更加精确。由于字符串可以包含不同大小写的相同字母，所以将其统一为小写可以避免不必要的错误。

#为什么程序员要这么做？ 
程序员使用「toLowerCase」可以简化代码，使得字符串比较更简单，从而提高效率。此外，许多编程语言中都有类似的功能，在学习和使用其他语言时，也可以快速掌握。

#如何使用： 
```PHP
$string = "HeLLo WOrlD"; //定义一个字符串 
echo strtolower($string); //输出字符串的小写版本：hello world
```

#深入了解： 
「toLowerCase」这一概念可以追溯到早期的计算机操作系统，当时字符仅能以大写格式被储存。随着计算机的发展，字符的大小写区分也变得可行。除了「toLowerCase」，一些编程语言也有类似的函数，如「strtoupper」和「ucfirst」等。

此外，有些编程语言使用「toLower」来实现相同的功能，只是命名不同。无论是哪种形式，都可以达到将字符串转换为小写的目的。

#参考链接： 
- PHP官方文档：https://www.php.net/manual/en/function.strtolower.php 
- Java中文网：https://docs.ruanjiadeng.com/java-tutorial/PHP/String-to-Lowercase-with-strtolower.htm 
- Python中文网：https://www.runoob.com/python/att-string-lower.html