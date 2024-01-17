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

## 什么是字符串连接？为什么程序员要这么做？

字符串连接指的是把两个或多个字符串合并成一个更长的字符串。程序员之所以这么做，是因为在编程中经常需要将多个字符串拼接在一起，以便创建新的文本内容或格式化输出。

## 如何进行字符串连接：

```PHP
$string1 = "Hello";
$string2 = "World";

echo $string1 . " " . $string2;
// Output: Hello World
```
```PHP
$name = "Mark";
$age = 25;

echo "My name is " . $name . ", and I am " . $age . " years old.";
// Output: My name is Mark, and I am 25 years old.
```

## 深入了解：

1. 字符串连接并不是PHP特有的功能，其他编程语言也有类似的功能。
2. 除了使用`.`来连接字符串外，也可以使用`sprintf()`函数或字符串插值符号`$`来实现相同的效果。
3. 在PHP中，字符串连接实际上是通过使用.运算符来实现的，.运算符不仅可以连接字符串，也可以连接其他类型的数据。

## 参考资料：

- [PHP字符串连接](https://www.php.net/manual/en/language.operators.string.php)
- [PHP中的sprintf()函数](https://www.php.net/manual/en/function.sprintf.php)
- [字符串插值符号$](https://www.php.net/manual/en/language.types.string.php#language.types.string.parsing)