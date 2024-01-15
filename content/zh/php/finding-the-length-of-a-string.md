---
title:                "寻找字符串的长度"
html_title:           "PHP: 寻找字符串的长度"
simple_title:         "寻找字符串的长度"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## 为什么

字符串是编程中经常用到的数据类型，它代表着一串字符。在PHP中，我们经常需要获取字符串的长度。这样做可以帮助我们检查输入的字符串是否符合特定的要求，或者处理字符串的截取或替换等操作。因此，了解如何找到字符串的长度是非常重要的。

## 如何进行

```PHP
$string = "This is a string.";
$length = strlen($string);
echo $length;
```

在上面的代码中，我们首先定义了一个字符串变量 `$string`，并将其赋值为 "This is a string."。然后，我们使用 `strlen()` 函数来获取字符串的长度，并将其赋值给变量 `$length`。最后，我们使用 `echo` 函数来输出 `$length` 的值。运行这段代码，我们可以看到输出结果为 `16`，即字符串 "This is a string." 的长度。

## 深入探讨

在PHP中，我们可以使用 `strlen()` 函数来获取不同类型的数据的长度，不仅仅是字符串。例如，我们也可以用来获取数组或者对象的长度。此外，PHP中还有一个类似的函数 `mb_strlen()`，它可以用来处理多字节字符，比如中文。

除了使用内置的函数，我们也可以使用循环来遍历字符串，并通过计数的方式求得字符串的长度。这种方式可能会比较复杂和低效，但是了解原理也是非常有用的。

## 参考资料

[PHP string length: Finding the length of a string](https://www.w3schools.com/php/func_string_strlen.asp)

[PHP: strlen - Manual](https://www.php.net/manual/en/function.strlen.php)

## 参见

[PHP字符串函数](https://www.php.net/manual/en/ref.strings.php)

[在PHP中使用数组](https://www.php.net/manual/en/language.types.array.php)