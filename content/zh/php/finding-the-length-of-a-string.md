---
title:    "PHP: 发现字符串的长度"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## 为什么

在编写PHP程序时，经常会遇到需要确定字符串长度的情况。这是因为字符串是使用最频繁的数据类型之一，掌握如何找到字符串长度可以帮助程序员更有效地操作和处理字符串。

## 如何操作

PHP提供了一个内建的函数`strlen()`来计算字符串的长度。让我们来看一个简单的例子：

```PHP
<?php
    $str = "Hello world!";
    echo strlen($str);
?>
```
输出结果为：12

在这个例子中，我们定义了一个字符串变量`$str`，然后使用`strlen()`函数来计算它的长度，并用`echo`语句输出结果。

除了直接使用`strlen()`函数，还可以通过将字符串转换成数组，然后使用数组的`count()`函数来计算字符串的长度。例如：

```PHP
<?php
    $str = "This is a string";
    $arr = str_split($str);
    echo count($arr);
?>
```
输出结果为：16

## 深入了解

当我们使用`strlen()`函数来计算字符串的长度时，函数实际上是通过对字符串中的每个字符进行计数来确定长度的。所以，这意味着`strlen()`函数也可以用来计算字符串中特定字符的数量。例如：

```PHP
<?php
    $str = "Hello World!";
    echo strlen($str) - strlen("l");
?>
```
输出结果为：10

这个例子中，我们用字符串的总长度减去字符串中字母"l"的数量，来得到字符串中除"l"外其他字符的数量。这显示了`strlen()`函数的灵活性。

## 参考链接

- [PHP字符串函数手册 - strlen()](https://www.php.net/manual/zh/function.strlen)
- [PHP数组函数手册 - count()](https://www.php.net/manual/zh/function.count)
- [PHP字符串转换成数组 - str_split()](https://www.php.net/manual/zh/function.str-split)