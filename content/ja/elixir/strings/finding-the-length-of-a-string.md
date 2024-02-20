---
date: 2024-01-20 17:47:22.194435-07:00
description: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u77E5\u308B\u306E\u306F\u57FA\
  \u672C\u3002\u30E1\u30E2\u30EA\u4F7F\u7528\u91CF\u3001\u30C7\u30FC\u30BF\u69CB\u9020\
  \u306E\u30D0\u30EA\u30C7\u30FC\u30B7\u30E7\u30F3\u3001\u30E6\u30FC\u30B6\u30FC\u5165\
  \u529B\u306E\u51E6\u7406\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:00.875946
model: gpt-4-1106-preview
summary: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u77E5\u308B\u306E\u306F\u57FA\
  \u672C\u3002\u30E1\u30E2\u30EA\u4F7F\u7528\u91CF\u3001\u30C7\u30FC\u30BF\u69CB\u9020\
  \u306E\u30D0\u30EA\u30C7\u30FC\u30B7\u30E7\u30F3\u3001\u30E6\u30FC\u30B6\u30FC\u5165\
  \u529B\u306E\u51E6\u7406\u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002"
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
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
