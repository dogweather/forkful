---
title:                "PHP: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

こんにちは、プログラマーの皆さん！今日はPHPの話題について書きたいと思います。今回のテーマは、「文字列を小文字に変換する方法」です。

## Why
なぜ誰かが文字列を小文字に変換する必要があるのでしょうか？それは、大文字と小文字を区別する必要がない場合や、文字列の比較をする必要がある場合など、さまざまな理由があります。例えば、ユーザーからの入力をチェックする際に、大文字と小文字を区別してチェックすると、誤った入力を受け付けてしまう可能性があります。そのため、入力された文字列をすべて小文字に変換することで、入力のバリデーションを正しく行うことができます。

## How To
PHPでは、文字列を小文字に変換するための組み込み関数が用意されています。それが、`strtolower()`です。以下のように使用することで、文字列を小文字に変換することができます。

```PHP
$str = "HELLO";
echo strtolower($str);
```

これは、以下のような出力になります。

```PHP
hello
```

また、演算子を使用しても同じ結果を得ることができます。

```PHP
$str = "HELLO";
echo $str . strtolower($str);
```

これは、以下のような出力になります。

```PHP
HELLOhello
```

## Deep Dive
文字列を小文字に変換する際に気を付けなければいけない点として、多言語の文字に対応する必要があるということです。通常、英語の文字では大きな問題は起こりませんが、他の言語を使用する場合には、文字の大文字と小文字が異なることがあります。そのため、`strtolower()`関数では、ロケールを指定することができます。例えば、日本語を対象とする場合には、以下のようにロケールを設定します。

```PHP
$str = "こんにちは";
echo strtolower($str, 'ja_JP');
```

出力には、以下のように全ての文字が小文字に変換されていることがわかります。

```
こんにちは
```

このように、`strtolower()`関数を使用する際には、文字のロケールについても考慮する必要があります。

## See Also
- [PHP: strtolower](https://www.php.net/manual/ja/function.strtolower.php)
- [PHPの小文字への変換](https://techacademy.jp/magazine/29531)
- [PHPのロケールについて](https://www.webcreatorbox.com/tech/php-locale)

それでは、今日の記事が役に立つことを願っています。また次の記事でお会いしましょう！