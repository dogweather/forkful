---
title:                "文字列の長さを求める"
date:                  2024-01-20T17:48:05.050536-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/finding-the-length-of-a-string.md"
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
