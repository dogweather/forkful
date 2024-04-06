---
date: 2024-01-20 17:46:07.301397-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.011457-06:00'
model: gpt-4-1106-preview
summary: "How to (\u5982\u4F55\u64CD\u4F5C) \u63D0\u53D6\u5B50\u5B57\u7B26\u4E32\u5728\
  PHP\u65E9\u671F\u5C31\u5B58\u5728\u4E86\uFF0C\u4E3B\u8981\u901A\u8FC7`substr`\u51FD\
  \u6570\u5B9E\u73B0\u3002\u5BF9\u4E8E\u591A\u8BED\u8A00\u7684\u6587\u672C\uFF0C\u5982\
  \u4E2D\u6587\uFF0C\u6700\u597D\u4F7F\u7528`mb_substr`\u6765\u4FDD\u8BC1\u6B63\u786E\
  \u5904\u7406\u3002`mb_substr`\u662F\u591A\u5B57\u8282\u5B57\u7B26\u4E32\u51FD\u6570\
  \u7684\u4E00\u90E8\u5206\uFF0C\u5141\u8BB8\u5B89\u5168\u5730\u8FDB\u884C\u5B50\u5B57\
  \u7B26\u4E32\u64CD\u4F5C\uFF0C\u4E0D\u7BA1\u5B57\u7B26\u7684\u7F16\u7801\u5982\u4F55\
  \u3002\u9664\u4E86`substr`\u548C`mb_substr`\uFF0C\u4F60\u8FD8\u53EF\u4EE5\u7528\
  `strstr`\u6216\u6B63\u5219\u8868\u8FBE\u5F0F\uFF0C\u4F46\u8FD9\u4E9B\u65B9\u6CD5\
  \u6709\u4E0D\u540C\u7684\u7528\u4F8B\u3002"
title: "\u63D0\u53D6\u5B50\u5B57\u7B26\u4E32"
weight: 6
---

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
