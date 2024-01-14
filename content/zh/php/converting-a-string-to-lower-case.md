---
title:                "PHP: 将字符串转换为小写"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要将字符串转换为小写

字符串转换为小写在实际的程序开发中非常常见。它可以用来统一字符串的格式，方便比较和搜索，也可以增强用户体验。无论是处理用户输入，还是从数据库或其他来源获取数据，我们都需要对字符串进行格式处理。将字符串转换为小写就是其中一种处理方式。

## 如何实现

首先，我们需要使用PHP内置的函数`strtolower()`来将字符串转换为小写。它接受一个字符串作为参数，并返回转换为小写的结果。让我们来看一个例子：

```PHP
$str = "Hello World";
echo strtolower($str);
```
输出为：
```
hello world
```

如上所示，我们可以直接使用`strtolower()`函数来转换字符串，并将结果打印出来。你也可以将转换后的结果赋值给一个新的变量，并在后续的代码中使用。此外，`strtolower()`函数也可以处理中文字符串。

## 深入探讨

在程序开发中，我们经常会将用户输入的字符串转换为小写，以便进行统一的比较。例如，用户在登录时，输入的用户名可能大小写不规范，那么我们就需要将其转换为小写后与数据库中的用户名进行比较。这样就可以确保即使用户输入的大小写不同，也能成功登录。

另外，当我们从数据库或其他来源获取数据时，也需要进行字符串转换。防止由于大小写不同导致无法正确匹配数据。字符串转换为小写也可以在搜索功能中发挥作用，以确保搜索结果的准确性。

## 参考链接
- [PHP官方文档：strtolower函数](http://php.net/manual/zh/function.strtolower.php)
- [PHP教程：转换大小写](http://www.w3school.com.cn/php/func_string_strtolower.asp)
- [理解 PHP 中的字符串操作函数](https://segmentfault.com/a/1190000002918641) 

## 参见

请参考上面提供的参考链接，了解更多关于处理字符串的方法。