---
date: 2024-01-20 17:46:16.958993-07:00
description: 'How to: .'
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.230520-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
weight: 6
---

## How to:
```PHP
<?php
$fullString = "こんにちは、世界！";
$substring = substr($fullString, 7, 2); // "世"を取り出す

echo $substring; // 出力：世
?>
```

```PHP
<?php
$anotherString = "PHPは楽しいです";
$extraction = mb_substr($anotherString, 3, 4); // "楽しい"を取り出す

echo $extraction; // 出力：楽しい
?>
```

## Deep Dive
サブストリングは文字列の小さな切り片です。`substr()`関数はPHP 4以降で使われていますが、`mb_substr()`はマルチバイト文字（例えば日本語など）を扱うためにあります。英語や数字のようなシングルバイト文字だけでなく、UTF-8などのマルチバイト文字セットでエンコードされた文字列でも正確に機能します。この違いは、非英語圏のプログラマーに特に重要です。

## See Also
- [PHP: substr - Manual](https://www.php.net/manual/en/function.substr.php)
- [PHP: mb_substr - Manual](https://www.php.net/manual/en/function.mb-substr.php)
- [PHP Multibyte String](https://www.php.net/manual/en/book.mbstring.php)
