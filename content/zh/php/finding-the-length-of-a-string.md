---
title:                "PHP: 检索字符串的长度"
simple_title:         "检索字符串的长度"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

一些时候，在编程中，我们需要知道字符串的长度。比如，我们可能需要限制用户输入的字符串的长度，或者对输入进行格式验证。因此，了解如何找到字符串的长度是很重要的。

## 如何

找到字符串的长度其实很简单。我们可以使用`strlen()`函数来获取字符串的长度。下面是一个简单的例子：

```PHP
$string = 'Hello World';
echo strlen($string); // 输出：11
```

在上面的代码中，`strlen()`函数接受一个字符串作为参数，并返回字符串的长度。在这个例子中，我们使用`echo`语句来输出结果，但您也可以把结果赋值给一个变量来后续使用。

此外，`strlen()`函数也可以处理中文字符。每个中文字符在UTF-8编码中占据3个字节，所以如果你的字符串包含中文，它的长度将会是每个中文字符数的三倍。

```PHP
$string = '你好世界';
echo strlen($string); // 输出：12
```

## 深入

在编程中，一个最基本的操作就是操纵字符串。而任何关于字符串操作的功能，都必须先获取字符串的长度。在PHP中，我们可以使用`strlen()`函数来找到字符串的长度，但除此之外，PHP还提供了很多其他的函数来操作字符串，比如`trim()`函数用来移除字符串末尾的空白字符，`substr()`函数用来截取字符串的一部分。这些函数都可以帮助我们更加灵活地操作字符串。

另外，值得一提的是，除了使用`strlen()`函数来获取字符串的长度，在某些情况下也可以通过`mb_strlen()`来获取更加精确的长度。这个函数是针对多字节字符的，比如中文，能够确保每个中文字符都被正确计算。但不论是使用`strlen()`还是`mb_strlen()`，只要您理解它们各自的特点，都能帮助您更好地处理字符串。

## 参见

- [PHP字符串函数](https://www.php.net/manual/en/ref.strings.php)
- [深入解析PHP中的字符串操作函数](https://www.php.net/manual/en/ref.strings.php)
- [UTF-8编码规则](https://www.w3schools.com/charsets/ref_utf8.asp)