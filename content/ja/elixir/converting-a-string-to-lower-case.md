---
title:    "Elixir: 「文字列を小文字に変換する」"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換するのか、その理由をご紹介します。

Elixirプログラミングでは、文字列を扱うことがよくあります。その中でも、文字列を小文字に変換することはよく行われる処理の一つです。例えば、入力された文字列を正しく比較するために、大文字や小文字を統一する必要があったり、文字列の表示を統一されたスタイルにするために必要だったりします。そのため、Elixirプログラミングを行う際に、文字列を小文字に変換する必要が生じることがあります。

## 方法
文字列を小文字に変換するには、Elixirの標準ライブラリである`String.downcase/1`を使用します。下記のように、変換したい文字列を引数として渡すことで、小文字に変換された文字列を返すことができます。

```Elixir
str = "HELLO WORLD!"
String.downcase(str)
```

実行結果は、`"hello world!"`になります。また、`String.downcase/1`は引数として文字列だけでなく、文字列のリストやバイナリを受け取ることもできます。

```Elixir
list = ["UpPeR", "LoWeR"]
String.downcase(list)
```
```Elixir
bin = <<72, 69, 76, 76, 79>>
String.downcase(bin)
```

実行結果は、`["upper", "lower"]`および`"hello"`になります。

## 深堀り
文字列を小文字に変換する際に、気をつけることがいくつかあります。まず、ElixirはUnicodeをサポートしているため、文字列はいかなる言語や文字でも扱うことができます。そのため、小文字に変換する際には、`utf8`オプションを使用することで、Unicode文字を正しく扱うようにしましょう。

また、`String.downcase/1`は文字列を不変的に扱うため、新しい文字列を返すようになっています。そのため、元の文字列を変更することができないことに注意しましょう。

## 併せて参照
- [Elixirの文字列操作](https://elixir-lang.org/ja/getting-started/basic-types.html#strings)
- [Unicodeのサポート](https://elixir-lang.org/ja/getting-started/unicode-charlists-and-binaries.html)
- [よく使うElixirの標準ライブラリ](https://elixirschool.com/ja/lessons/basics/basics/#%E3%82%88%E3%81%8F%E4%BD%BF%E3%81%86%E3%80%8Belixir%E3%81%AE%E6%A8%99%E6%BA%96%E3%83%A9%E3%82%A4%E3%83%96%E3%83%A9%E3%83%AA%EF%BC%88%E4%BB%8A%E6%9B%B8%E3%81%84%E3%81%BE%E3%81%99%E3%81%A7%E8%A7%A3%E8%AA%AC%E4%B8%AD%EF%BC%89)

以上を参考に、Elixirで文字列を小文字に変換する方法をマスターしましょう。さまざまな処理で文字列を扱うことがあるので、このような基本的な操作を覚えておくことは大切です。

## 併せて参照