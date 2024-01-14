---
title:                "Elixir: 文字列を小文字に変換する"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ

文字列を小文字に変換するのにかかる時間をかける価値はありますか？

文字列を小文字に変換することには様々な理由があります。例えば、検索や比較を行う場合、文字列の大文字と小文字を区別することは大きな違いを生み出すことがあります。また、データベースに格納する際に全ての文字列を小文字に統一することで、一貫性を保つことができます。

## 方法

Elixirでは、`String.downcase/1`関数を使用することで簡単に文字列を小文字に変換することができます。以下の例をご覧ください。

```Elixir
iex> String.downcase("HELLO")
"hello"
```

また、文字列の一部を小文字に変換する場合は、`String.replace/3`関数を使用することもできます。

```Elixir
iex> String.replace("Hello World!", "World", String.downcase("World"))
"Hello world!"
```

## 深堀り

`String.downcase/1`関数は、文字列を小文字に変換する際にASCIIの範囲内の文字のみを対象にします。これはElixirの文字列がUTF-8で表現されているためです。しかし、日本語の文字を小文字に変換する場合は、`String.downcase/2`関数を使用して、任意のUnicodeのマップを渡すことができます。

```Elixir
iex> String.downcase("こんにちは", "ja")
"こんにちは"
```

## 参考リンク

- [Elixir公式ドキュメント - String.downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)
- [Elixir公式ドキュメント - String.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
- [ASCIIとUnicodeの違いについて](https://qiita.com/chihiro/items/5b934f5ee0ca7863ab94)
- [UTF-8とは何か？](https://qiita.com/osocorporation/items/3bd0844fe2e793bdf349)