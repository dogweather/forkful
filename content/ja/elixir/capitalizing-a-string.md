---
title:                "文字列を大文字にする"
html_title:           "Elixir: 文字列を大文字にする"
simple_title:         "文字列を大文字にする"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ?

文字列の大文字化は、各単語の最初の文字を大文字に、残りを小文字に変換する操作です。プログラマーはこれを行うことで、テキストデータが一貫性を保ち、人間にとってより読みやすくなるからです。

## やり方：

Elixirでは、`String.capitalize/2` 関数を使用して文字列を大文字化します。例を見てみましょう：

```elixir
iex> String.capitalize("elixir programming")
"Elixir Programming"
```
先頭の各単語が大文字になり、残りの文字は小文字になりました。

## 深堀り：

Elixirの`String.capitalize/2`関数はユニコードに関する情報に基づいて大文字化を行いますので、様々な言語と文字コードでも信頼性があります。また、この関数は古いものではなく、Elixirの初期バージョンから存在します。

大文字化の代替手段として、全てを大文字にする`String.upcase/1`または全てを小文字にする`String.downcase/1`があります。これらは特定のケースで役立つ方法ですが、混合ケースの文字列にはあまり適していません。

大文字化の実装について深く見ると、`String.capitalize/2`関数は、空白文字を境界として単語を区切り、各単語の最初の文字を`String.upcase/1`で、残りの文字を`String.downcase/1`で処理して塊を再組み立てます。この詳細はElixirのソースコードで確認できます。

## その他参考情報：

以下のリンクからElixirの`capitalize/2`、`upcase/1`、`downcase/1`関係する情報について詳しく見ることができます：

- Elixir公式ドキュメンテーション: [capitalize/2](https://hexdocs.pm/elixir/String.html#capitalize/2)
- Elixir公式ドキュメンテーション: [upcase/1](https://hexdocs.pm/elixir/String.html#upcase/1)
- Elixir公式ドキュメンテーション: [downcase/1](https://hexdocs.pm/elixir/String.html#downcase/1)