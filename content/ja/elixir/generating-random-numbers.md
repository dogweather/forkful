---
title:                "ランダム数の生成"
date:                  2024-01-20T17:48:57.378946-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ランダム数の生成は予測不可能な数字を作ることです。プログラマーがテストデータ、ゲーム、セキュリティなどで使います。

## How to: (やり方)
Elixirでランダムな数を生成するのは簡単です。`:rand`モジュールを使ってみましょう。

```elixir
# 0から999の間のランダムな整数を生成
random_integer = :rand.uniform(1000) - 1
IO.puts(random_integer)

# 1から6の間のサイコロの目を生成
dice_roll = :rand.uniform(6)
IO.puts(dice_roll)
```

サンプル出力は実行するたびに変わります。例えば：
```
537
4
```

## Deep Dive (掘り下げ)
Elixirでは、`:rand`モジュールはErlangの`:rand`を使ってランダム数を生成します。':rand.uniform/1'関数が一様分布のランダム値を生成します。歴史的には、他のアプローチとして`random`モジュールがありましたが、それは非推奨とされています。`:rand`は再現可能な乱数シーケンスとして、シードを指定することもできます。また、`:crypto.strong_rand_bytes`を使って、暗号学的に安全なランダム数を生成することもできますが、一般的な用途では`:rand`が十分です。

## See Also (関連情報)
- Erlangの`:rand`モジュールドキュメント: [http://erlang.org/doc/man/rand.html](http://erlang.org/doc/man/rand.html)
- 暗号学的に安全なランダム数の`:crypto`モジュール: [http://erlang.org/doc/man/crypto.html](http://erlang.org/doc/man/crypto.html)