---
date: 2024-01-20 17:48:08.376065-07:00
description: "How to: (\u5982\u4F55\u64CD\u4F5C\uFF1A) \u6CE8\u610F\u5728UTF-8\u7F16\
  \u7801\u4E0B\uFF0C\u4E2D\u6587\u5B57\u7B26\u53EF\u80FD\u88AB\u8BA1\u7B97\u4E3A\u591A\
  \u4E2A\u5B57\u8282\u3002\u8981\u51C6\u786E\u8BA1\u7B97\u5B57\u7B26\u6570\uFF0C\u8BF7\
  \u4F7F\u7528 `mb_strlen`\u3002"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.163063-06:00'
model: gpt-4-1106-preview
summary: "(\u5982\u4F55\u64CD\u4F5C\uFF1A) \u6CE8\u610F\u5728UTF-8\u7F16\u7801\u4E0B\
  \uFF0C\u4E2D\u6587\u5B57\u7B26\u53EF\u80FD\u88AB\u8BA1\u7B97\u4E3A\u591A\u4E2A\u5B57\
  \u8282\u3002\u8981\u51C6\u786E\u8BA1\u7B97\u5B57\u7B26\u6570\uFF0C\u8BF7\u4F7F\u7528\
  \ `mb_strlen`\u3002"
title: "\u83B7\u53D6\u5B57\u7B26\u4E32\u7684\u957F\u5EA6"
weight: 7
---

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
