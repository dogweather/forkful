---
date: 2024-01-20 17:47:22.194435-07:00
description: "How to: (\u65B9\u6CD5) Elixir\u3067\u6587\u5B57\u5217\u306E\u9577\u3055\
  \u3092\u898B\u3064\u3051\u308B\u306B\u306F`String.length/1`\u95A2\u6570\u3092\u4F7F\
  \u3044\u307E\u3059\u3002Unicode\u6587\u5B57\u5217\u306B\u3082\u5BFE\u5FDC\u3057\u3066\
  \u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.600927-06:00'
model: gpt-4-1106-preview
summary: "Elixir\u3067\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u898B\u3064\u3051\
  \u308B\u306B\u306F`String.length/1`\u95A2\u6570\u3092\u4F7F\u3044\u307E\u3059\u3002\
  Unicode\u6587\u5B57\u5217\u306B\u3082\u5BFE\u5FDC\u3057\u3066\u3044\u307E\u3059."
title: "\u6587\u5B57\u5217\u306E\u9577\u3055\u3092\u6C42\u3081\u308B"
weight: 7
---

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
