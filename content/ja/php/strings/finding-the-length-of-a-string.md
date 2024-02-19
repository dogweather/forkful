---
aliases:
- /ja/php/finding-the-length-of-a-string/
date: 2024-01-20 17:48:05.050536-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3068\
  \u306F\u3001\u305D\u306E\u6587\u5B57\u5217\u306B\u542B\u307E\u308C\u308B\u6587\u5B57\
  \u306E\u6570\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u306E\u691C\u8A3C\u3001\u5165\u529B\u306E\
  \u51E6\u7406\u3001\u307E\u305F\u306F\u7279\u5B9A\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u306B\u5408\u308F\u305B\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u5B9F\u884C\
  \u3057\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.987336
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\u308B\u3068\
  \u306F\u3001\u305D\u306E\u6587\u5B57\u5217\u306B\u542B\u307E\u308C\u308B\u6587\u5B57\
  \u306E\u6570\u3092\u6570\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u30C7\u30FC\u30BF\u306E\u691C\u8A3C\u3001\u5165\u529B\u306E\
  \u51E6\u7406\u3001\u307E\u305F\u306F\u7279\u5B9A\u306E\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u306B\u5408\u308F\u305B\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u5B9F\u884C\
  \u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
---

{{< edit_this_page >}}

## What & Why? (なにとなぜ？)
文字列の長さを見つけるとは、その文字列に含まれる文字の数を数えることです。プログラマーはデータの検証、入力の処理、または特定のフォーマットに合わせるためにこれを実行します。

## How to: (やり方)
PHPにおいて文字列の長さを取得するのは簡単です。`strlen()` 関数を使います。例を見てみましょう。

```php
<?php
$string = "こんにちは";
$length = strlen($string);
echo $length; // 出力は15です。なぜなら、"こんにちは"はマルチバイト文字だからです。
?>
```

マルチバイト文字列の場合、`mb_strlen()` 関数を使いましょう。

```php
<?php
$multiByteString = "こんにちは";
$length = mb_strlen($multiByteString);
echo $length; // 出力は5です。マルチバイト対応です。
?>
```

## Deep Dive (深掘り)
`strlen()` 関数はPHP 4より前から存在し、単純な1バイト文字列に対して使われてきました。ですが、マルチバイト文字(例えば日本語のような)の場合、`mb_strlen()` 関数が必要です。なぜなら、`strlen()` はバイト数を数えるのに対し、`mb_strlen()` は実際の文字数を数えるためです。設定や環境による実行速度の差もありますが、一般に`mb_strlen()` の方が少し遅いです。PHPを使う際は、文字エンコーディングに注意し、適切な関数を選択することが重要です。

## See Also (参考文献)
- [PHPの`strlen`関数](https://www.php.net/manual/en/function.strlen.php)
- [PHPの`mb_strlen`関数](https://www.php.net/manual/en/function.mb-strlen.php)
- [PHPマルチバイト文字列](https://www.php.net/manual/en/book.mbstring.php)
