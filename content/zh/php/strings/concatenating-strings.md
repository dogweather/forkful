---
date: 2024-01-20 17:35:29.285216-07:00
description: "\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u628A\u4E24\u4E2A\u6216\u591A\
  \u4E2A\u5B57\u7B26\u4E32\u5408\u5E76\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\u6784\u9020\u6570\u636E\u3001\u751F\u6210\
  \u52A8\u6001\u5185\u5BB9\u6216\u8005\u62FC\u63A5\u8DEF\u5F84\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:47.854562-06:00'
model: gpt-4-1106-preview
summary: "\u5B57\u7B26\u4E32\u62FC\u63A5\u5C31\u662F\u628A\u4E24\u4E2A\u6216\u591A\
  \u4E2A\u5B57\u7B26\u4E32\u5408\u5E76\u6210\u4E00\u4E2A\u3002\u7A0B\u5E8F\u5458\u8FD9\
  \u4E48\u505A\u4E3B\u8981\u662F\u4E3A\u4E86\u6784\u9020\u6570\u636E\u3001\u751F\u6210\
  \u52A8\u6001\u5185\u5BB9\u6216\u8005\u62FC\u63A5\u8DEF\u5F84\u3002."
title: "\u5B57\u7B26\u4E32\u62FC\u63A5"
weight: 3
---

## What & Why?
字符串拼接就是把两个或多个字符串合并成一个。程序员这么做主要是为了构造数据、生成动态内容或者拼接路径。

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
