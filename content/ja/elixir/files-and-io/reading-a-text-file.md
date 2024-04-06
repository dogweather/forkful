---
date: 2024-01-20 17:54:11.107931-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.042451-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u30C6\u30AD\u30B9\u30C8\u30D5\u30A1\u30A4\u30EB\u306E\u8AAD\u307F\u8FBC\u307F"
weight: 22
---

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
