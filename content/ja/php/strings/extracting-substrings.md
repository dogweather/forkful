---
title:                "部分文字列の抽出"
aliases:
- /ja/php/extracting-substrings.md
date:                  2024-01-20T17:46:16.958993-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/extracting-substrings.md"
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
