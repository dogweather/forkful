---
title:                "字符串首字母大写"
date:                  2024-01-19
html_title:           "Arduino: 字符串首字母大写"
simple_title:         "字符串首字母大写"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? 什么是以及为什么？

在PHP中，字符串大写是把所有字母转换成大写形式。程序员这么做为了格式统一，例如把用户输入规范化，让用户界面看起来更专业。

## How to: 如何操作：

### 使用 `strtoupper()` 转换整个字符串为大写：

```php
<?php
$originalString = "hello, 世界!";
$upperCaseString = strtoupper($originalString);
echo $upperCaseString; // 输出 HELLO, 世界!
?>
```

### 使用 `mb_strtoupper()` 转换包含多字节字符的字符串为大写：

```php
<?php
$originalString = "hello, 世界!";
$upperCaseString = mb_strtoupper($originalString);
echo $upperCaseString; // 输出 HELLO, 世界!
?>
```

注意：`mb_strtoupper()` 是 `strtoupper()` 的多字节字符串版本。

## Deep Dive 深入探索

在PHP的早期版本中，`strtoupper()` 仅支持单字节字符集。后来引入了 `mbstring` 扩展，它提供了对多字节字符集（如UTF-8）更好的支持。如果你用的是多字节字符（比如中文、日文、韩文等），`mb_strtoupper()` 将确保所有字符都正确地转换成大写，而无需担心字符编码问题。

还有其他方法可以进行大小写转换，如 `strtolower()` 将字符串变为小写，而 `ucfirst()` 和 `ucwords()` 将字符串的第一个字母或每个单词的首字母变为大写。

当实现字符串大写时，考虑到性能和正确性是重要的。通常，使用 `strtoupper()` 或 `mb_strtoupper()` 就可以满足大多数需求，但如果你需要针对特定语言进行大小写规则的转换，可能需要查找或实现特定语言的大小写映射函数。

## See Also 查看更多

- [PHP 官方文档 - strtoupper](https://www.php.net/manual/en/function.strtoupper.php)
- [PHP 官方文档 - mb_strtoupper](https://www.php.net/manual/en/function.mb-strtoupper.php)
- [PHP 官方文档 - mbstring 扩展](https://www.php.net/manual/en/book.mbstring.php)
