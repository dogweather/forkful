---
title:    "PHP: 寻找字符串的长度"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/zh/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么
在编写PHP程序时，经常会遇到需要计算字符串长度的情况。知道字符串的长度可以帮助我们更有效地处理和操作数据，因此掌握如何计算字符串长度是非常重要的。

## 如何
计算字符串长度的最简单方法是使用内置函数`strlen()`。让我们来看一个简单的例子：
```PHP
<?php
$string = "Hello World";
$length = strlen($string);
echo $length; // 输出：11
?>
```
这里，我们首先定义一个字符串变量，然后使用`strlen()`函数来获取它的长度，并将长度存储在变量`$length`中，最后使用`echo`语句将长度输出到屏幕上。

我们也可以使用循环来计算字符串长度，例如：
```PHP
<?php
$string = "Hello World";
$length = 0;
for($i = 0; $string[$i] != NULL; $i++) {
    $length++;
}
echo $length; // 输出：11
?>
```
在这个例子中，我们使用一个循环来遍历字符串中的每个字符，并通过计数器`$length`来记录字符的数量。当循环结束时，计数器的值就是字符串长度。

## 深入探讨
在PHP中，字符串的长度实际上是指字符串中包含的字节数。这也意味着，如果字符串中包含多字节字符（如中文），那么它的长度将大于实际字符数。

另外，`strlen()`函数也适用于数组和对象。当传入数组时，它将返回数组元素的数量，当传入对象时，它将返回对象属性的数量。

## 参考链接
- [PHP `strlen()`函数文档](https://www.php.net/manual/zh/function.strlen.php)
- [PHP 字符串处理指南](https://www.php.net/manual/zh/book.strings.php)
- [逐字符地检索字符串](https://www.php.net/manual/zh/function.mb-substr.php)

## 参见
- [PHP 数组长度如何计算](https://www.example.com/p/array-length-php)
- [如何处理多字节字符的字符串长度](https://www.example.com/p/string-length-multibyte-php)