---
title:                "提取子字符串"
date:                  2024-01-20T17:46:07.301397-07:00
model:                 gpt-4-1106-preview
simple_title:         "提取子字符串"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (是什么 & 为什么？)

在PHP中提取子字符串就是从一个字符串中获取部分内容。程序员经常这样做来分析数据，验证输入，或者简化字符串操作。

## How to (如何操作)

```PHP
<?php
$text = "Hello, PHP programmers!";

// 使用substr
$greeting = substr($text, 0, 5);
echo $greeting; // 输出: Hello

// 使用mb_substr (对多字节字符友好)
$mb_greeting = mb_substr($text, 7, 3);
echo $mb_greeting; // 输出: PHP
?>
```

## Deep Dive (深入了解)

提取子字符串在PHP早期就存在了，主要通过`substr`函数实现。对于多语言的文本，如中文，最好使用`mb_substr`来保证正确处理。`mb_substr`是多字节字符串函数的一部分，允许安全地进行子字符串操作，不管字符的编码如何。除了`substr`和`mb_substr`，你还可以用`strstr`或正则表达式，但这些方法有不同的用例。

## See Also (另请参阅)

- PHP官方文档中的substr: https://www.php.net/manual/en/function.substr.php
- PHP官方文档中的mb_substr: https://www.php.net/manual/en/function.mb-substr.php
- 更多字符串处理方法: https://www.php.net/manual/en/ref.strings.php
