---
title:                "「文字列の長さを求める」"
html_title:           "Elixir: 「文字列の長さを求める」"
simple_title:         "「文字列の長さを求める」"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることに関わる意義や、その重要性について説明します。これは、それがどのように開発者の生産性やコードのパフォーマンスに影響を与えるかをお伝えするためです。Elixirで文字列の長さを求めることで、コードの調整や最適化のための情報を得ることができ、より効率的なプログラミングを行うことができます。

## 使い方

まず、Elixirの `String.length` 関数を使うことで文字列の長さを簡単に取得することができます。以下は、単純な文字列の長さを取得する例です。

```Elixir
string = "Hello World"
IO.puts "Length: #{String.length(string)}"
```
このコードを実行すると、コンソールには `Length: 11` という結果が表示されます。また、文字列内の漢字や絵文字のようなマルチバイト文字も正しくカウントされます。

さらに、Elixirではパイプライン演算子を使うことで、より見やすくコードを組み立てることができます。例えば、文字数を数える前に、文字列をトリムしたい場合は以下のように書けます。

```Elixir
string = " Hello World "
string
|> String.trim()
|> String.length()
|> IO.puts("Length: #{String.length(string)}")
```

## ディープダイブ

文字列の長さを求めるには、内部的にどのような処理が行われるのでしょうか。実は、Elixirでは文字列をバイトのリストとして扱うため、長さを取得する際には文字列のバイト数をカウントする必要があります。

しかし、Elixirの `String.length` 関数はUTF-8のマルチバイト文字についても適切に処理されるように最適化されています。つまり、文字数をカウントするのに必要なバイト数だけをカウントするのではなく、正しい文字数を返すようになっています。

## もっと詳しく知りたい方へ

もしもっとElixirの文字列操作について学びたい方は、以下のリンクを参考にしてください。

- [ElixirのStringモジュールドキュメント](https://hexdocs.pm/elixir/String.html)
- [Elixirラーニングガイド - 文字列操作](https://elixir-lang.org/getting-started/string.html)
- [Elixirスタイルガイド - 文字列操作](https://github.com/christopheradams/elixir_style_guide#strings)

## 関連リンク

- [Elixir公式サイト](https://elixir-lang.org/)
- [Elixirドキュメント](https://hexdocs.pm/elixir/Kernel.html)