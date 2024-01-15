---
title:                "ランダムな数値の生成"
html_title:           "Elixir: ランダムな数値の生成"
simple_title:         "ランダムな数値の生成"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数値を生成することに興味があると思われる方々にとって、Elixirは非常に優れたツールです。Elixirの強力なランダム数生成機能を使うことで、より多くの可能性を持ったプログラムを作ることができます。

## 方法

まず、ElixirのライブラリであるRandomをインポートします。次に、Randomを使用して任意の形式のランダム数を生成する方法を示します。

```elixir
iex> import Random
iex> rand() # デフォルトでは0から1までの浮動小数点数を返す
0.7801630443077161

iex> rand(1..10) # 引数を渡すことで最小値と最大値を指定できる
6

iex> rand(1.0..5.5) # 浮動小数点数の範囲も指定できる
4.891950072786604

iex> rand(1..10, 5) # 第二引数で生成する数の個数を指定できる
[7, 2, 9, 4, 3]
```

また、ランダムな文字列を生成することもできます。

```elixir
iex> rand_string = Enum.shuffle("abcdefghijklmnopqrstuvwxyz") |> List.first(5) |> Enum.join("") # アルファベットのランダムな5文字の文字列を生成する
"uqklm"

iex> rand_string = Enum.shuffle(["apple", "banana", "cherry"]) |> List.first() # リストからランダムに要素を取得することもできる
"banana"
```

## 深堀り

内部的には、ElixirのRandomはRNG（乱数生成器）モジュールに基づいています。RNGは疑似乱数を生成しますが、それでも実用的な乱数を生成できるように設計されています。また、乱数のシード値を手動で指定することもできます。

## 関連情報

- [ElixirのRandomドキュメント](https://hexdocs.pm/elixir/Random.html)
- [RubyKaigi 2014：ランダムでErlang を卒業する (John Hainsworth)](https://www.youtube.com/watch?v=4SH_-aI_1ho)
- [Elixirで疑似乱数がランダムにならない場合の原因と対処法](https://qiita.com/khsk/items/e1b0ebf731fd4fd24a66)