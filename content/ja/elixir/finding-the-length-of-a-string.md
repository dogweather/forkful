---
title:                "文字列の長さを求める"
date:                  2024-01-20T17:47:22.194435-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
文字列の長さを知るのは基本。メモリ使用量、データ構造のバリデーション、ユーザー入力の処理に不可欠です。

## How to: (方法)
Elixirで文字列の長さを見つけるには`String.length/1`関数を使います。Unicode文字列にも対応しています。

```elixir
# 文字列の長さを取得
string = "こんにちは"
length = String.length(string)
IO.puts(length)  # 出力: 5
```

短いサンプルながら、以上が全てです。

## Deep Dive (探求)
長年、文字列の長さを調べることは、多くのプログラミング言語の基本機能でした。Elixirでは、文字列はUTF-8でエンコードされており、`String.length/1`は正確な文字単位での長さを返します。

古い言語では、ASCII文字のみを扱ったため、文字数はバイト数と等しかった。しかし、全角文字や絵文字など、複数のバイトが必要なUnicode文字には、`String.length/1`が便利。

他の手段としては、バイナリ表現のバイトサイズを測る`byte_size/1`がありますが、通常は文字列の実際の長さを得たい場合には使われません。

```elixir
# バイトサイズを測る
byte_size = byte_size(string)
IO.puts(byte_size)  # 出力: 15
```

上記の場合、"こんにちは"は15バイトですが、5文字です。

## See Also (参照)
- Elixir公式ドキュメントの[Stringモジュール](https://hexdocs.pm/elixir/String.html)
- [Programming Elixir](https://pragprog.com/titles/elixir16/programming-elixir-1-6/) - プログラミングの基本から先進的なテクニックまでカバーしています。
- [Elixir School](https://elixirschool.com/jp/) - Elixirの基礎に特化した無料の学習リソースです。