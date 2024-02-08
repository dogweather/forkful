---
title:                "テキストファイルの読み込み"
aliases:
- ja/elixir/reading-a-text-file.md
date:                  2024-01-20T17:54:11.107931-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストファイルの読み込み"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/reading-a-text-file.md"
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
