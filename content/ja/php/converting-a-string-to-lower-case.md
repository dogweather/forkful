---
title:                "「文字列を小文字に変換する」"
html_title:           "PHP: 「文字列を小文字に変換する」"
simple_title:         "「文字列を小文字に変換する」"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列を小文字に変換することは、プログラマーがしばしば行う操作です。この操作を行う理由は、大文字や小文字の違いを無視して文字列を比較するためです。例えば、ユーザーが入力したパスワードを正しく認証するためには、入力された文字列を同じ形式に変換する必要があります。そのため、文字列を小文字に変換することは、プログラマーにとって非常に便利な操作といえます。

## 方法:

`PHP strtolower()` 関数を使用することで、文字列を簡単に小文字に変換することができます。以下の例をご覧ください。

```PHP
$input = "Hello, WORLD!";
$output = strtolower($input);
echo $output;
```
**出力:**
```
hello, world!
```
また、`mb_strtolower()` 関数を使用することで、マルチバイト文字にも対応した小文字変換が可能です。以下の例をご覧ください。

```PHP
$input = "こんにちは、WORLD!";
$output = mb_strtolower($input);
echo $output;
```
**出力:**
```
こんにちは、world!
```

## 深堀り:

文字列を小文字に変換する操作は、古くからプログラミング言語に存在する基本的な記述です。また、多くの言語では `strtolower()` などの組み込み関数が提供されているため、手軽に利用することができます。

しかしながら、一部の言語では文字コードやロケールの違いによって、小文字変換の動作が異なる場合があります。そのため、異なる環境で動作するアプリケーションを開発する場合は、この点に注意する必要があります。

## 関連リンク:

- [PHPの strtolower() 関数](https://www.php.net/manual/ja/function.strtolower.php)
- [PHPの mb_strtolower() 関数](https://www.php.net/manual/ja/function.mb-strtolower.php)
- [パスワードの比較における大文字小文字の扱いについて](https://www.kaoriya.net/blog/2011/01/27/)