---
title:    "Elixir: ランダム数字の生成"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

Elixirでランダムな数字を生成する方法

## なぜ
ランダムな数字を生成するのにElixirを利用する理由は多数あります。例えば、ゲームやシミュレーション、セキュリティーなど、多くのアプリケーションで乱数が必要になります。また、Elixirの乱数生成機能は非常に強力であり、高速かつ信頼性の高い結果を提供します。

## ハウツー
```Elixir
# 1から10までのランダムな整数を生成する例
rand = Enum.random(1..10)
IO.puts(rand)
# 出力例: 7
```

```Elixir
# 0から1までのランダムな浮動小数点数を生成する例
rand = :random.uniform()
IO.puts(rand)
# 出力例: 0.7726834950140793
```

Elixirにはランダムな数値を生成するために使用できる多数の関数があります。また、特定の範囲内の乱数を生成するための便利なヘルパー関数も存在します。

## ディープダイブ
Elixirでは、乱数を生成するための独自のアルゴリズムが使用されています。このアルゴリズムは、統計的に均一な分布を保証するために設計されており、サイドエフェクトや競合条件を防ぐために並列処理に対応しています。また、乱数生成のためのシード値を指定することもできます。

## 参考リンク
- [Elixirの乱数生成機能について](https://elixir-lang.org/docs/master/elixir/Kernel.html#rand/1)
- [Erlangの乱数生成機能について](http://erlang.org/doc/man/random.html)
- [乱数の範囲を制限するには？](https://elixirforum.com/t/how-to-generate-random-numbers-within-a-specific-range/9618)

## 参考にする
この記事ではElixirで乱数を生成する方法について学びました。乱数はさまざまなアプリケーションで必要になる重要な機能です。Elixirの乱数生成機能は非常に強力であるため、ぜひ積極的に活用してください。