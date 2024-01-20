---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# PHP（現行バージョン）で文字列を小文字に変換する方法

## 何 & なぜ？
文字列を小文字に変換するとは、すべての大文字を対応する小文字に変換することです。プログラマは、テキスト比較や検索を大文字と小文字を区別しないために行います。

## どうやって：
PHPで文字列をすべて小文字に変換する最も直接的な方法は、ビルトインの```strtolower```関数を使うことです。
```PHP
$text = "Hello World!";
$lower_case_text = strtolower($text);
echo $lower_case_text;
```
これにより以下の出力が得られます：
```
hello world!
```

## 深堀り：
#### ■ 歴史的背景
PHPで最初に文字列を小文字に変換する能力が導入されたのは、最初の公開版（PHP/FI 2.0）の```strtolower```関数からです。

#### ■ 代替案
```mb_strtolower```関数はマルチバイト文字列（例：日本語や他の多くの非ラテン文字）を小文字に変換するために使用することができます。

#### ■ 実装詳細
```strtolower```関数は内部的にCの```tolower```関数を使って実装されています。これはASCII文字の小文字変換のみをサポートしています。

## 参考情報：
より深く理解するためには以下のリンクを参照してください。


1. PHP公式マニュアル：[strtolower](https://www.php.net/manual/ja/function.strtolower.php)
2. PHP公式マニュアル：[mb_strtolower](https://www.php.net/manual/ja/function.mb-strtolower.php)