---
date: 2024-01-20 17:42:49.627834-07:00
description: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\
  \u5B57\u7B26\u610F\u5473\u7740\u6839\u636E\u7279\u5B9A\u89C4\u5219\u79FB\u9664\u5B57\
  \u7B26\u4E32\u4E2D\u7684\u67D0\u4E9B\u90E8\u5206\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u53EF\u4EE5\u6E05\u7406\u6570\u636E\u3001\u683C\u5F0F\u5316\u8F93\u51FA\u6216\
  \u8005\u5728\u5904\u7406\u6587\u672C\u65F6\u4FDD\u7559\u9700\u8981\u7684\u4FE1\u606F\
  \u3002"
isCJKLanguage: true
lastmod: '2024-03-11T00:14:21.634504-06:00'
model: gpt-4-1106-preview
summary: "\u5728\u7F16\u7A0B\u4E2D\uFF0C\u5220\u9664\u5339\u914D\u6A21\u5F0F\u7684\
  \u5B57\u7B26\u610F\u5473\u7740\u6839\u636E\u7279\u5B9A\u89C4\u5219\u79FB\u9664\u5B57\
  \u7B26\u4E32\u4E2D\u7684\u67D0\u4E9B\u90E8\u5206\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\
  \u505A\u53EF\u4EE5\u6E05\u7406\u6570\u636E\u3001\u683C\u5F0F\u5316\u8F93\u51FA\u6216\
  \u8005\u5728\u5904\u7406\u6587\u672C\u65F6\u4FDD\u7559\u9700\u8981\u7684\u4FE1\u606F\
  \u3002"
title: "\u5339\u914D\u6A21\u5F0F\u5220\u9664\u5B57\u7B26"
---

{{< edit_this_page >}}

## What & Why? (是什么？为什么？)
在编程中，删除匹配模式的字符意味着根据特定规则移除字符串中的某些部分。程序员这样做可以清理数据、格式化输出或者在处理文本时保留需要的信息。

## How to: (怎么做？)
在PHP中，我们使用`preg_replace()`函数来删除匹配正则表达式模式的字符。看下面的例子：

```php
<?php
$text = "Hello, 2023!";
$pattern = '/\d+/'; // 匹配所有数字
$replacement = '';
$newText = preg_replace($pattern, $replacement, $text);

echo $newText; // 输出 "Hello, !"
?>
```
用这种方式，我们删除了字符串中的所有数字。

## Deep Dive (深入了解)
删除字符串中匹配特定模式的字符在PHP中有很悠久的历史。`preg_replace()`是现代PHP程序中最常用的方法，但`ereg_replace()`等旧函数也曾被用于相似的目的，只是现在已经不推荐使用了。

除了`preg_replace()`，你也可以使用`str_replace()`或者`str_ireplace()`等函数删除特定字符，但它们没有正则表达式那样的灵活性。

关于实现细节，`preg_replace()`使用PCRE（Perl Compatible Regular Expression）库来处理正则表达式，这是很多现代编程语言共有的标准库。

## See Also (另请参阅)
- [preg_replace() documentation](https://www.php.net/manual/en/function.preg-replace.php)
- [Regular Expressions (Perl-Compatible)](https://www.php.net/manual/en/reference.pcre.pattern.syntax.php)
