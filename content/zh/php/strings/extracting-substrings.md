---
date: 2024-01-20 17:46:07.301397-07:00
description: "\u5728PHP\u4E2D\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\
  \u4E00\u4E2A\u5B57\u7B26\u4E32\u4E2D\u83B7\u53D6\u90E8\u5206\u5185\u5BB9\u3002\u7A0B\
  \u5E8F\u5458\u7ECF\u5E38\u8FD9\u6837\u505A\u6765\u5206\u6790\u6570\u636E\uFF0C\u9A8C\
  \u8BC1\u8F93\u5165\uFF0C\u6216\u8005\u7B80\u5316\u5B57\u7B26\u4E32\u64CD\u4F5C\u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.639834-06:00'
model: gpt-4-1106-preview
summary: "\u5728PHP\u4E2D\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5C31\u662F\u4ECE\u4E00\
  \u4E2A\u5B57\u7B26\u4E32\u4E2D\u83B7\u53D6\u90E8\u5206\u5185\u5BB9\u3002\u7A0B\u5E8F\
  \u5458\u7ECF\u5E38\u8FD9\u6837\u505A\u6765\u5206\u6790\u6570\u636E\uFF0C\u9A8C\u8BC1\
  \u8F93\u5165\uFF0C\u6216\u8005\u7B80\u5316\u5B57\u7B26\u4E32\u64CD\u4F5C\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
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
