---
title:                "Elixir: テキストの検索と置き換え"
simple_title:         "テキストの検索と置き換え"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ 

プログラミングの世界でテキストを検索＆置換することの重要性はとても大きいです。特に、一度に大量のテキストを簡単に変更したい場合にとても有用です。Elixirでは、このタスクを効率的に実行することができる方法がたくさんありますので、今回はその中でも特にその方法についてご紹介します。

## 使い方

Elixirでは、`String.replace/3`という関数を使うことで、簡単にテキストの検索と置換ができます。この関数には、3つの引数があります。

- 第1引数：検索対象のテキスト（String型）
- 第2引数：検索したい文字列（もしくは正規表現）（String型）
- 第3引数：置換したい文字列（もしくは置換用の関数）（String型もしくは引数が1つの関数）

例えば、以下のコードでは、「apple」という文字列を「orange」に置換しています。

```Elixir
iex> String.replace("I love apple pie!", "apple", "orange")
"I love orange pie!"
```

また、正規表現を使うことで、複数の文字列を一括で置換することもできます。

```Elixir
iex> String.replace("1 + 2 = 3", ~r/[0-9]/, "X")
"X + X = X"
```

関数を使うことで、より柔軟な置換が可能になります。以下の例では、`String.upcase/1`という関数を使って、すべての文字を大文字に変換しています。

```Elixir
iex> String.replace("Hello, world!", "Hello", fn(word) -> String.upcase(word) end)
"HELLO, WORLD!"
```

## 深堀り

先ほどご紹介した`String.replace/3`以外にも、検索と置換を行うための関数がたくさん用意されています。

- `String.replace_leading/3`：先頭から検索して置換する
- `String.replace_trailing/3`：末尾から検索して置換する
- `String.replace_anywhere/3`：すべての文字列から検索して置換する

また、Elixirでは正規表現を使うこともできます。正規表現を使うことで、より高度な検索と置換が可能になります。

さらに、Elixirではパイプライン演算子を使うことで、複数の文字列を一連の処理としてつなげることができます。これにより、より簡潔で読みやすいコードを書くことができます。

## おわりに

今回は、Elixirを使ってテキストの検索と置換を行う方法についてご紹介しました。Elixirには多くの文字列関数が用意されており、パイプライン演算子と組み合わせることで、より効率的かつ柔軟な処理を実現することができます。ぜひ、ぜひお試しください！

## 関連リンク

- [Elixir 公式ドキュメント] (https://hexdocs.pm/elixir/String.html)
- [Elixir公式サイト] (https://elixir-lang.org/)
- [Elixir入門サイト] (https://elixir.bootcamp/lessons/strings)