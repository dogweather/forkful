---
title:                "字符串插值"
date:                  2024-01-20T17:51:11.576811-07:00
model:                 gpt-4-1106-preview
simple_title:         "字符串插值"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/php/interpolating-a-string.md"
---

{{< edit_this_page >}}

## 什么和为什么？
字符串插值是一种将变量值嵌入到字符串中的技巧。程序员这样做是为了构建动态内容，提升代码的可读性和灵活性。

## 如何操作：
```PHP
<?php
$user = '张三';
$text = "欢迎, $user!";
echo $text; // 输出: 欢迎, 张三!

// 复杂结构使用花括号
$cart = ['apple' => '苹果'];
echo "我想购买的是: {$cart['apple']}。"; // 输出: 我想购买的是: 苹果。
?>
```

## 深入了解
在PHP中，字符串插值始于变量前的美元符号($)。只有双引号或heredoc结构中的字符串才能解析变量。PHP的字符串插值能力省去了频繁的字符串拼接，这在早期的编程语言中不常见。

除了简单变量之外，复杂变量（如数组元素和对象属性）可以通过花括号`{}`嵌入。这有助于解决复杂表达式的歧义问题。

PHP中的替代方法包含了使用点(.)运算符来连接字符串，但当涉及到多个变量和固定文本时，这样会更加繁琐。

## 参考资料
- [PHP: 字符串 - 手册](https://www.php.net/manual/zh/language.types.string.php)
- [PHP: Heredoc 语法 - 手册](https://www.php.net/manual/zh/language.types.string.php#language.types.string.syntax.heredoc)