---
title:                "将字符串转换为小写"
aliases: - /zh/php/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:13.941580-07:00
model:                 gpt-4-1106-preview
simple_title:         "将字符串转换为小写"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
将字符串转换为小写意味着把所有大写字母改为小写形式。程序员这样做来统一数据格式、提高比较数据时的准确性，或是满足特定编程要求。

## How to: (如何操作：)
PHP 转换字符串为小写可以用 `strtolower()` 函数。给你看几个简单的例子：

```PHP
<?php
$exampleText = "Hello, World!";
echo strtolower($exampleText); // 输出: hello, world!

// 使用 UTF-8 字符
$chineseText = "PHP编程!";
echo strtolower($chineseText); // 输出依赖于具体环境和字符集配置
?>
```
注意：非英文字符的结果可能依赖于你的环境和字符集配置。

## Deep Dive (深入了解)
在历史上，字符串的大小写转换起初只适用于英文字符。随着编程国际化，`strtolower()` 函数通过 `locale` 设置支持多种语言，不过效果不总是理想。备选方案如 `mb_strtolower()` 用于多字节字符串，对 UTF-8 等编码更友好。

```PHP
// 使用多字节字符串函数
$exampleText = "Привет мир!";
echo mb_strtolower($exampleText, 'UTF-8'); // 输出: привет мир!
```

在 PHP 内部，`strtolower()` 和 `mb_strtolower()` 实现不同。`strtolower()` 逐字符处理，速度快但在非单字节字符集可能出错。`mb_strtolower()` 考虑了字符编码，兼容性更好，适用范围更广。

## See Also (另请参阅)
- PHP 官方文档中的 `strtolower()`：[https://www.php.net/manual/en/function.strtolower.php](https://www.php.net/manual/en/function.strtolower.php)
- PHP 官方文档中的 `mb_strtolower()`：[https://www.php.net/manual/en/function.mb-strtolower.php](https://www.php.net/manual/en/function.mb-strtolower.php)
- UTF-8 和字符编码基础：[https://www.php.net/manual/en/book.mbstring.php](https://www.php.net/manual/en/book.mbstring.php)
