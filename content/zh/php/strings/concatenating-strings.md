---
date: 2024-01-20 17:35:29.285216-07:00
description: "How to: \u65E9\u671F\u7684PHP\u7248\u672C\u4E2D\uFF0C\u5B57\u7B26\u4E32\
  \u62FC\u63A5\u4E3B\u8981\u901A\u8FC7`.`\u8FD0\u7B97\u7B26\u3002\u8FD9\u5F88\u7B80\
  \u5355\u9AD8\u6548\u3002\u968F\u7740PHP\u7684\u7248\u672C\u8FED\u4EE3\uFF0C\u5C24\
  \u5176\u662F\u5728PHP 8\u53CA\u4E4B\u540E\u7684\u7248\u672C\u4E2D\uFF0C\u5B57\u7B26\
  \u4E32\u5904\u7406\u53D8\u5F97\u66F4\u52A0\u5F3A\u5927\u548C\u7075\u6D3B\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:38:47.014789-06:00'
model: gpt-4-1106-preview
summary: "\u65E9\u671F\u7684PHP\u7248\u672C\u4E2D\uFF0C\u5B57\u7B26\u4E32\u62FC\u63A5\
  \u4E3B\u8981\u901A\u8FC7`.`\u8FD0\u7B97\u7B26\u3002\u8FD9\u5F88\u7B80\u5355\u9AD8\
  \u6548\u3002\u968F\u7740PHP\u7684\u7248\u672C\u8FED\u4EE3\uFF0C\u5C24\u5176\u662F\
  \u5728PHP 8\u53CA\u4E4B\u540E\u7684\u7248\u672C\u4E2D\uFF0C\u5B57\u7B26\u4E32\u5904\
  \u7406\u53D8\u5F97\u66F4\u52A0\u5F3A\u5927\u548C\u7075\u6D3B\u3002"
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

## How to:
```PHP
<?php
// 简单的字符串拼接
$greeting = "你好, ";
$name = "世界!";
$welcome = $greeting . $name;

echo $welcome; // 输出: 你好, 世界!

// 使用变量直接拼接
$sentence = "欢迎 " . $name . " 阅读这篇文章。";
echo $sentence; // 输出: 欢迎 世界! 阅读这篇文章。

// 使用花括号进行复杂拼接
$order = 1;
$message = "{$greeting}顾客{$order}号";
echo $message; // 输出: 你好, 顾客1号
?>
```

## Deep Dive
早期的PHP版本中，字符串拼接主要通过`.`运算符。这很简单高效。随着PHP的版本迭代，尤其是在PHP 8及之后的版本中，字符串处理变得更加强大和灵活。

除了直接使用`.`运算符外，复合字符串写法（例如使用双引号包围的变量）也可以用来拼接字符串。不过，直接使用`.`来拼接字符串在性能上通常更好，尤其是在处理大量的字符串操作时。

在拼接过程中，需要注意字符编码的一致性，特别是在处理多语言环境（比如中英文混编）时。

另外，相关函数如`implode()`可以用来连接数组中的字符串，`sprintf()`或`printf()`可以用于格式化字符串拼接。

谨慎使用这些拼接方法，过度拼接会导致代码难以维护，尤其是在构造复杂的SQL查询或大段HTML时。

## See Also
- [PHP字符串处理手册](https://www.php.net/manual/zh/book.strings.php)
- [PHP `implode()` 函数](https://www.php.net/manual/zh/function.implode.php)
- [PHP `sprintf()` 函数](https://www.php.net/manual/zh/function.sprintf.php)
