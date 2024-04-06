---
date: 2024-01-20 17:54:11.107931-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T22:37:49.963769-06:00'
model: gpt-4-1106-preview
summary: "How to (\u65B9\u6CD5) Elixir\u306E\u30D5\u30A1\u30A4\u30EB\u8AAD\u307F\u8FBC\
  \u307F\u306FErlang\u3067\u7BC9\u304B\u308C\u3066\u304A\u308A\u3001\u5927\u91CF\u306E\
  \u30C7\u30FC\u30BF\u51E6\u7406\u306B\u7406\u60F3\u7684\u3067\u3059\u3002\u30B9\u30AF\
  \u30EA\u30FC\u30E0\u306F\u30E1\u30E2\u30EA\u52B9\u7387\u304C\u826F\u304F\u3001\u5927\
  \u304D\u306A\u30D5\u30A1\u30A4\u30EB\u3082\u30D0\u30C3\u30C1\u3067\u51E6\u7406\u3059\
  \u308B\u3053\u3068\u304C\u3067\u304D\u307E\u3059\u3002`File.read/1`\u3084`File.readlines/1`\u306E\
  \u3088\u3046\u306A\u5225\u306E\u95A2\u6570\u3082\u4F7F\u3048\u307E\u3059\u304C\u3001\
  `File.stream!/3`\u306F\u9045\u5EF6\u8AAD\u307F\u8FBC\u307F\u3067\u52B9\u7387\u7684\
  \u306A\u64CD\u4F5C\u3092\u63D0\u4F9B\u3057\u307E\u3059\u3002\u3053\u306E\u6A5F\u80FD\
  \u306F\u7279\u306B\u30E9\u30A4\u30D6\u30C7\u30FC\u30BF\u3084\u5927\u898F\u6A21\u306A\
  \u30ED\u30B0\u30D5\u30A1\u30A4\u30EB\u3092\u6271\u3046\u3068\u304D\u306B\u4FBF\u5229\
  \u3067\u3059\u3002"
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
