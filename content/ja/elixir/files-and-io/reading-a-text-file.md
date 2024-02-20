---
date: 2024-01-20 17:54:11.107931-07:00
description: "\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u30C6\u30AD\u30B9\u30C8\u3092\u8AAD\
  \u3080\u306E\u306F\u30C7\u30FC\u30BF\u306E\u62BD\u51FA\u3084\u51E6\u7406\u306B\u5FC5\
  \u9808\u3067\u3059\u3002Elixir\u3067\u306F\u7C21\u5358\u3067\u30A8\u30EC\u30AC\u30F3\
  \u30C8\u306B\u884C\u3048\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: 2024-02-19 22:05:00.911300
model: gpt-4-1106-preview
summary: "\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u30C6\u30AD\u30B9\u30C8\u3092\u8AAD\
  \u3080\u306E\u306F\u30C7\u30FC\u30BF\u306E\u62BD\u51FA\u3084\u51E6\u7406\u306B\u5FC5\
  \u9808\u3067\u3059\u3002Elixir\u3067\u306F\u7C21\u5358\u3067\u30A8\u30EC\u30AC\u30F3\
  \u30C8\u306B\u884C\u3048\u307E\u3059\u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

ファイルからテキストを読むのはデータの抽出や処理に必須です。Elixirでは簡単でエレガントに行えます。

## How to (方法)
```elixir
# テキストファイルを開く
File.stream!("example.txt") 
|> Enum.each(fn line -> IO.puts(line) end)

# サンプル出力:
# これはテキストファイルの例の一行目です。
# 二行目だよ。
# そして、これが三行目です。
```

## Deep Dive (深い潜在)

Elixirのファイル読み込みはErlangで築かれており、大量のデータ処理に理想的です。スクリームはメモリ効率が良く、大きなファイルもバッチで処理することができます。`File.read/1`や`File.readlines/1`のような別の関数も使えますが、`File.stream!/3`は遅延読み込みで効率的な操作を提供します。この機能は特にライブデータや大規模なログファイルを扱うときに便利です。

## See Also (参照)

- Elixir公式ドキュメント: [File](https://hexdocs.pm/elixir/File.html)
- Elixir School: [ファイル処理](https://elixirschool.com/jp/lessons/basics/collections#ファイル処理)
- エリック・メイヤーの関数型プログラミングの原理: [関数型プログラミング](https://www.haskell.org/tutorial/) (Elixirも関数型言語に分類されます。)
