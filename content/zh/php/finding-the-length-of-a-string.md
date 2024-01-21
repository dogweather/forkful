---
title:                "获取字符串的长度"
date:                  2024-01-20T17:48:08.376065-07:00
model:                 gpt-4-1106-preview
simple_title:         "获取字符串的长度"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (是什么与为什么？)
找出字符串的长度就是测量它有多少个字符。程序员需要这信息来验证数据（比如密码强度）、限制输入、或优化存储。

## How to: (如何操作：)
```PHP
<?php
$string = "你好世界";
echo strlen($string);  // 输出: 12
?>
```
注意在UTF-8编码下，中文字符可能被计算为多个字节。要准确计算字符数，请使用 `mb_strlen`。

```PHP
<?php
$string = "你好世界";
echo mb_strlen($string, "UTF-8");  // 输出: 4
?>
```

## Deep Dive (深入探究)
在早期PHP版本中，`strlen()` 函数足以处理大多数字符串长度问题。但随着多语言支持的引入，单个字符可能占多个字节（比如UTF-8编码的中文字符），这就需要 `mb_strlen` 来准确处理。对于不同编码的字符串，始终使用 `mb_strlen`，并明确指定编码。

## See Also (另请参阅)
- PHP官方文档关于 `strlen`: https://www.php.net/manual/en/function.strlen.php 
- PHP官方文档关于 `mb_strlen`: https://www.php.net/manual/en/function.mb-strlen.php 
- PHP多字节字符串处理: https://www.php.net/manual/en/book.mbstring.php