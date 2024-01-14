---
title:                "Elixir: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラミングを学ぶと、文字列の長さを求めることができるようになります。このスキルは、日常的に使われるタスクの多くに応用することができます。そこで今回は、Elixirを使って文字列の長さを求める方法を紹介します。

## ハウツー

まずは、Stringモジュールの`length`関数を使って、文字列の長さを取得する方法を見ていきましょう。下のコードブロックを参考に、Elixirで文字列の長さを求めることができます。

```Elixir
str = "こんにちは、世界！"
len = String.length(str)

IO.puts("文字列 #{str} の長さは #{len} です")
```

上のコードを実行すると、次のような結果が得られます。

```
文字列 こんにちは、世界！ の長さは 9 です
```

次に、`String.length`の代わりに、文字列の長さを求めるためのカスタム関数を定義してみましょう。以下のコードブロックを参考に、関数を定義して呼び出すことができます。

```Elixir
def string_length(str) do
    count = 0
    for _ <- String.codepoints(str) do
        count = count + 1
    end
    count
end

IO.puts("文字列 #{str} の長さは #{string_length(str)} です")
```

この関数では、Stringモジュールの`codepoints`関数を使って、文字列をコードポイント（Unicodeの文字単位）に分割し、その数をカウントしています。上のコードを実行すると、同じく9の結果が得られます。

## ディープダイブ

文字列の長さを求める方法にはいくつかのアプローチがありますが、最も効率的なのは`String.length`を使う方法です。この関数は、文字列がUTF-8であるかどうかに関わらず、正しい文字数を返します。また、同じ関数を使ってバイナリデータの長さを求めることもできます。

## ぜひ参考にしてみてください

"## それ以外" - https://hexdocs.pm/elixir/String.html#length/1