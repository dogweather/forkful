---
aliases:
- /zh/php/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:42.487385-07:00
description: "\u5728 PHP \u4E2D\u7684\u6B63\u5219\u8868\u8FBE\u5F0F\uFF08regex\uFF09\
  \u662F\u7528\u4E8E\u5339\u914D\u5B57\u7B26\u4E32\u4E2D\u5B57\u7B26\u7EC4\u5408\u7684\
  \u6A21\u5F0F\uFF0C\u5141\u8BB8\u8FDB\u884C\u590D\u6742\u7684\u641C\u7D22\u548C\u66FF\
  \u4EE3\u64CD\u4F5C\u4EE5\u53CA\u6570\u636E\u9A8C\u8BC1\u3002\u7A0B\u5E8F\u5458\u5229\
  \u7528\u6B63\u5219\u8868\u8FBE\u5F0F\u56E0\u5176\u5728\u89E3\u6790\u6587\u672C\u3001\
  \u9A8C\u8BC1\u8868\u5355\u6216\u6293\u53D6\u7F51\u9875\u6570\u636E\u7B49\u65B9\u9762\
  \u7684\u5F3A\u5927\u548C\u7075\u6D3B\u6027\uFF0C\u4F7F\u5176\u6210\u4E3A\u5F00\u53D1\
  \u8005\u5DE5\u5177\u7BB1\u4E2D\u4E0D\u53EF\u6216\u7F3A\u7684\u5DE5\u5177\u3002"
lastmod: 2024-02-18 23:08:59.206925
model: gpt-4-0125-preview
summary: "\u5728 PHP \u4E2D\u7684\u6B63\u5219\u8868\u8FBE\u5F0F\uFF08regex\uFF09\u662F\
  \u7528\u4E8E\u5339\u914D\u5B57\u7B26\u4E32\u4E2D\u5B57\u7B26\u7EC4\u5408\u7684\u6A21\
  \u5F0F\uFF0C\u5141\u8BB8\u8FDB\u884C\u590D\u6742\u7684\u641C\u7D22\u548C\u66FF\u4EE3\
  \u64CD\u4F5C\u4EE5\u53CA\u6570\u636E\u9A8C\u8BC1\u3002\u7A0B\u5E8F\u5458\u5229\u7528\
  \u6B63\u5219\u8868\u8FBE\u5F0F\u56E0\u5176\u5728\u89E3\u6790\u6587\u672C\u3001\u9A8C\
  \u8BC1\u8868\u5355\u6216\u6293\u53D6\u7F51\u9875\u6570\u636E\u7B49\u65B9\u9762\u7684\
  \u5F3A\u5927\u548C\u7075\u6D3B\u6027\uFF0C\u4F7F\u5176\u6210\u4E3A\u5F00\u53D1\u8005\
  \u5DE5\u5177\u7BB1\u4E2D\u4E0D\u53EF\u6216\u7F3A\u7684\u5DE5\u5177\u3002"
title: "\u4F7F\u7528\u6B63\u5219\u8868\u8FBE\u5F0F"
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 PHP 中的正则表达式（regex）是用于匹配字符串中字符组合的模式，允许进行复杂的搜索和替代操作以及数据验证。程序员利用正则表达式因其在解析文本、验证表单或抓取网页数据等方面的强大和灵活性，使其成为开发者工具箱中不可或缺的工具。

## 如何使用：

PHP 通过 PCRE（Perl 兼容的正则表达式）库支持正则表达式，提供了丰富的函数集。以下是如何使用它们：

### 匹配模式：

要检查字符串中是否存在模式，请使用 `preg_match()`。若字符串中找到了模式，此函数返回 1，否则返回 0。

```php
if (preg_match("/\bweb\b/i", "PHP 是一种 web 脚本语言")) {
    echo "找到了匹配。";
} else {
    echo "未找到匹配。";
}
// 输出：找到了匹配。
```

### 查找所有匹配项：

当需要在字符串中找到模式的所有出现时，使用 `preg_match_all()`。

```php
$text = "猫和狗";
$pattern = "/\b([a-z]+)\b/i";
preg_match_all($pattern, $text, $matches);
print_r($matches[0]);
// 输出：Array ( [0] => 猫 [1] => 和 [2] => 狗 )
```

### 替换文本：

要替换与正则表达式匹配的文本，使用 `preg_replace()`。它对于格式化和清理数据非常强大。

```php
$originalText = "2003年4月15日";
$pattern = "/(\w+) (\d+), (\d+)/i";
$replacement = '${1}1,$3';
echo preg_replace($pattern, $replacement, $originalText);
// 输出：2003年4月1,15
```

### 分割字符串：

你可以使用 `preg_split()` 将字符串分割成数组，指定一个模式作为分隔符。

```php
$text = "PHP 是, 一种极为流行的, 脚本语言";
$parts = preg_split("/,\s*/", $text);
print_r($parts);
// 输出：Array ( [0] => PHP 是 [1] => 一种极为流行的 [2] => 脚本语言 )
```

此外，对于复杂的正则表达式模式和任务，如 Symfony 的 `Finder` 组件或 Laravel 的帮助函数集合等框架和库可能提供更便利的抽象层。然而，理解和使用 PHP 内置的 PCRE 函数对于直接在 PHP 脚本中有效地进行文本处理和验证至关重要。
