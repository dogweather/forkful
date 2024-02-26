---
date: 2024-01-20 17:46:16.958993-07:00
description: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\u51FA\u3068\u306F\u3001\
  \u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u90E8\u5206\u6587\u5B57\u5217\u3092\
  \u53D6\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u30C7\u30FC\u30BF\u3092\u64CD\u4F5C\u3057\u305F\u308A\u3001\u30E6\u30FC\
  \u30B6\u30FC\u5165\u529B\u3092\u691C\u8A3C\u3057\u305F\u308A\u3001\u610F\u5473\u306E\
  \u3042\u308B\u90E8\u5206\u3092\u5206\u6790\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.232883-07:00'
model: gpt-4-1106-preview
summary: "\u30B5\u30D6\u30B9\u30C8\u30EA\u30F3\u30B0\u62BD\u51FA\u3068\u306F\u3001\
  \u6587\u5B57\u5217\u304B\u3089\u7279\u5B9A\u306E\u90E8\u5206\u6587\u5B57\u5217\u3092\
  \u53D6\u308A\u51FA\u3059\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\
  \u30FC\u306F\u30C7\u30FC\u30BF\u3092\u64CD\u4F5C\u3057\u305F\u308A\u3001\u30E6\u30FC\
  \u30B6\u30FC\u5165\u529B\u3092\u691C\u8A3C\u3057\u305F\u308A\u3001\u610F\u5473\u306E\
  \u3042\u308B\u90E8\u5206\u3092\u5206\u6790\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u90E8\u5206\u6587\u5B57\u5217\u306E\u62BD\u51FA"
---

{{< edit_this_page >}}

## What & Why?
サブストリング抽出とは、文字列から特定の部分文字列を取り出すことです。プログラマーはデータを操作したり、ユーザー入力を検証したり、意味のある部分を分析するためにこれを行います。

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
