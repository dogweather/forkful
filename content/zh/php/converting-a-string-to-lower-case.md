---
title:                "PHP: 将字符串转换为小写"
simple_title:         "将字符串转换为小写"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 为什么要将字符串转换为小写

在编程中，有时我们需要将字符串转换为小写形式。这可能是为了与数据库中的数据匹配，或者为了方便比较字符大小写。不管什么原因，学习如何将字符串转换为小写形式是非常重要的。

## 如何实现字符串转换为小写形式
```PHP
$string = "HELLO, WORLD";
echo strtoupper($string); // outputs "hello, world"
```
在上面的例子中，我们使用了PHP内置函数`strtoupper()`来将字符串转换为小写形式。这个函数接收一个字符串作为参数，并返回一个全部为小写字母的新字符串。

```PHP
$string = "ThIs iS a MixEd CaSe";
echo strtolower($string); // outputs "this is a mixed case"
```
另一个PHP内置函数`strtolower()`也能实现同样的功能。它将字符串中的所有字母转换为小写形式，并返回一个新的字符串。

## 深入了解字符串转换为小写形式
在PHP中，字符串的大小写转换是通过使用ASCII编码来实现的。ASCII编码是一种标准的字符集，它赋予每一个字符一个数字编码，从而使得计算机能够识别并处理这些字符。

在ASCII编码中，大写字母和小写字母是有着不同的数值范围，因此我们可以通过改变字符的ASCII编码来实现大小写转换。这也就是为什么我们能够使用`strtoupper()`和`strtolower()`这样的函数来进行转换。

## 参考文章

- [PHP官方文档 - strtoupper()](https://www.php.net/manual/en/function.strtoupper.php)
- [PHP官方文档 - strtolower()](https://www.php.net/manual/en/function.strtolower.php)
- [ASCII编码表](https://www.ascii-code.com/)