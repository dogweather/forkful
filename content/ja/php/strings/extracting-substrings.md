---
date: 2024-01-20 17:46:16.958993-07:00
description: "How to: \u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u306F\u6587\u5B57\
  \u5217\u306E\u5C0F\u3055\u306A\u5207\u308A\u7247\u3067\u3059\u3002`substr()`\u95A2\
  \u6570\u306FPHP\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T21:53:43.087273-06:00'
model: gpt-4-1106-preview
summary: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u306F\u6587\u5B57\u5217\u306E\
  \u5C0F\u3055\u306A\u5207\u308A\u7247\u3067\u3059\u3002`substr()`\u95A2\u6570\u306F\
  PHP 4\u4EE5\u964D\u3067\u4F7F\u308F\u308C\u3066\u3044\u307E\u3059\u304C\u3001`mb_substr()`\u306F\
  \u30DE\u30EB\u30C1\u30D0\u30A4\u30C8\u6587\u5B57\uFF08\u4F8B\u3048\u3070\u65E5\u672C\
  \u8A9E\u306A\u3069\uFF09\u3092\u6271\u3046\u305F\u3081\u306B\u3042\u308A\u307E\u3059\
  \u3002\u82F1\u8A9E\u3084\u6570\u5B57\u306E\u3088\u3046\u306A\u30B7\u30F3\u30B0\u30EB\
  \u30D0\u30A4\u30C8\u6587\u5B57\u3060\u3051\u3067\u306A\u304F\u3001UTF-8\u306A\u3069\
  \u306E\u30DE\u30EB\u30C1\u30D0\u30A4\u30C8\u6587\u5B57\u30BB\u30C3\u30C8\u3067\u30A8\
  \u30F3\u30B3\u30FC\u30C9\u3055\u308C\u305F\u6587\u5B57\u5217\u3067\u3082\u6B63\u78BA\
  \u306B\u6A5F\u80FD\u3057\u307E\u3059\u3002\u3053\u306E\u9055\u3044\u306F\u3001\u975E\
  \u82F1\u8A9E\u570F\u306E\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306B\u7279\u306B\u91CD\
  \u8981\u3067\u3059\u3002"
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
