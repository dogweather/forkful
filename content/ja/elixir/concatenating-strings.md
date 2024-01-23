---
title:                "文字列の連結"
date:                  2024-01-20T17:34:51.287450-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の連結"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の連結とは、２つ以上の文字列をくっつけ一つの文字列にすることです。プログラマーがこれを行う理由は、動的なテキストコンテンツを生成したり、ユーザー入力をフォーマットしたりするためです。

## How to: (方法)
```elixir
# 文字列を連結する基本的な方法
greeting = "こんにちは、"
name = "ユウキ"

message = greeting <> name
IO.puts message

# 出力: こんにちは、ユウキ
```

```elixir
# 文字列のリストを使用して連結する方法
parts = ["これは", " Elixirで", " 文字列を", " 連結する", " 例です。"]
full_message = Enum.join(parts)
IO.puts full_message

# 出力: これは Elixirで 文字列を 連結する 例です。
```

## Deep Dive (探究)
Elixirの文字列はUTF-8エンコーディングのバイナリデータです。`<>` 演算子は、Elixir 1.0から文字列を連結するために導入されました。他の言語と違い、`+` 演算子ではなく、意図的に異なる演算子が選ばれました。これは、`+`は数字の加算に使うべきという言語設計の哲学に基づいています。

他にも、`Enum.join/2` 関数を使ってリスト内の文字列をマージする手法もあります。このメソッドは、間に特定のセパレータを入れながら連結する際にも役立ちます。

文字列の効率的な処理はElixirにおいて大切です。Elixirは、Erlangのバイナリ処理の高効率性を活かしパフォーマンスの向上を図っています。

## See Also (関連項目)
- Elixirの公式ドキュメント：[Elixir Lang](https://elixir-lang.org/docs.html)
- `Enum.join/2`のドキュメント: [Enum.join/2](https://hexdocs.pm/elixir/Enum.html#join/2)
- 文字列操作に関する他の関数とモジュール: [String](https://hexdocs.pm/elixir/String.html)
