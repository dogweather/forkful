---
date: 2024-01-20 17:48:05.050536-07:00
description: "How to: (\u3084\u308A\u65B9) PHP\u306B\u304A\u3044\u3066\u6587\u5B57\
  \u5217\u306E\u9577\u3055\u3092\u53D6\u5F97\u3059\u308B\u306E\u306F\u7C21\u5358\u3067\
  \u3059\u3002`strlen()` \u95A2\u6570\u3092\u4F7F\u3044\u307E\u3059\u3002\u4F8B\u3092\
  \u898B\u3066\u307F\u307E\u3057\u3087\u3046\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.232557-06:00'
model: gpt-4-1106-preview
summary: "PHP\u306B\u304A\u3044\u3066\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u53D6\
  \u5F97\u3059\u308B\u306E\u306F\u7C21\u5358\u3067\u3059\u3002`strlen()` \u95A2\u6570\
  \u3092\u4F7F\u3044\u307E\u3059\u3002\u4F8B\u3092\u898B\u3066\u307F\u307E\u3057\u3087\
  \u3046."
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
