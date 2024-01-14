---
title:                "PHP: 计算字符串的长度"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# 为什么要学习如何找出字符串的长度？

在编程中，字符串是一种非常常见的数据类型，它由一系列的字符组成。在某些情况下，我们需要知道一个字符串的长度，比如判断一个字符串是否满足特定的条件，或者在处理字符串时需要限制其长度。因此，学习如何找出字符串的长度是非常重要的。

## 如何找出字符串的长度？

在PHP中，我们可以通过使用`strlen()`函数来找出一个字符串的长度。下面是一个简单的例子：

```PHP
$string = "Hello World!";
echo strlen($string); // 输出 12
```

另外，我们也可以通过遍历字符串的每一个字符来计算其长度。下面是使用`for`循环的例子：

```PHP
$string = "Hello World!";
$length = 0;

for ($i = 0; $i < strlen($string); $i++) {
    $length++;
}

echo $length; // 输出 12
```

## 深入了解字符串长度的计算方法

上面提到的`strlen()`函数是PHP内置的函数，用来计算字符串的长度。它实际上是通过遍历字符串中的每个字符，并统计出字符的数量来计算长度的。这也是为什么我们可以用`for`循环来手动计算字符串的长度的原因。

另外，对于一些特殊的字符，比如中文字符，可能会导致`strlen()`函数计算出的长度和实际长度不符。在这种情况下，可以使用`mb_strlen()`函数来进行正确的计算。该函数需要指定字符编码，比如UTF-8，以便正确计算字符串长度。

## 参考资料

了解字符串长度相关函数的更多信息，请参考以下资料：

- [PHP官方手册：strlen()函数](https://www.php.net/manual/en/function.strlen.php)
- [PHP官方手册：mb_strlen()函数](https://www.php.net/manual/en/function.mb-strlen.php)