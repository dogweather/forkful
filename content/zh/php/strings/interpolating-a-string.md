---
date: 2024-01-20 17:51:11.576811-07:00
description: "\u5982\u4F55\u64CD\u4F5C\uFF1A \u5728PHP\u4E2D\uFF0C\u5B57\u7B26\u4E32\
  \u63D2\u503C\u59CB\u4E8E\u53D8\u91CF\u524D\u7684\u7F8E\u5143\u7B26\u53F7($)\u3002\
  \u53EA\u6709\u53CC\u5F15\u53F7\u6216heredoc\u7ED3\u6784\u4E2D\u7684\u5B57\u7B26\u4E32\
  \u624D\u80FD\u89E3\u6790\u53D8\u91CF\u3002PHP\u7684\u5B57\u7B26\u4E32\u63D2\u503C\
  \u80FD\u529B\u7701\u53BB\u4E86\u9891\u7E41\u7684\u5B57\u7B26\u4E32\u62FC\u63A5\uFF0C\
  \u8FD9\u5728\u65E9\u671F\u7684\u7F16\u7A0B\u8BED\u8A00\u4E2D\u4E0D\u5E38\u89C1\u3002\
  \ \u9664\u4E86\u7B80\u5355\u53D8\u91CF\u4E4B\u5916\uFF0C\u590D\u6742\u53D8\u91CF\
  \uFF08\u5982\u6570\u7EC4\u5143\u7D20\u548C\u5BF9\u8C61\u5C5E\u6027\uFF09\u53EF\u4EE5\
  \u901A\u8FC7\u82B1\u62EC\u53F7`{}`\u5D4C\u5165\u3002\u8FD9\u6709\u52A9\u4E8E\u89E3\
  \u51B3\u590D\u6742\u8868\u8FBE\u5F0F\u7684\u6B67\u4E49\u95EE\u9898\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:48.158177-06:00'
model: gpt-4-1106-preview
summary: "\u9664\u4E86\u7B80\u5355\u53D8\u91CF\u4E4B\u5916\uFF0C\u590D\u6742\u53D8\
  \u91CF\uFF08\u5982\u6570\u7EC4\u5143\u7D20\u548C\u5BF9\u8C61\u5C5E\u6027\uFF09\u53EF\
  \u4EE5\u901A\u8FC7\u82B1\u62EC\u53F7`{}`\u5D4C\u5165\u3002\u8FD9\u6709\u52A9\u4E8E\
  \u89E3\u51B3\u590D\u6742\u8868\u8FBE\u5F0F\u7684\u6B67\u4E49\u95EE\u9898."
title: "\u5B57\u7B26\u4E32\u63D2\u503C"
weight: 8
---

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
