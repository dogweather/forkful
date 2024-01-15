---
title:                "ランダムな数字の生成"
html_title:           "Haskell: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

偶数の生成に関わることにどのような理由があるのでしょうか？実は、ランダムな数値はゲームやシミュレーションなどで非常に重要な役割を果たします。例えば、カードゲームのようにプレイヤーにランダムな手札を配る必要がある場合や、自然現象の挙動をシミュレーションする必要がある場合などにランダムな数値を生成する必要があります。

## 方法

Haskellでは、ランダムな数値を生成するために `random` というモジュールが利用可能です。まずは以下のようにモジュールをインポートしてみましょう。

```Haskell
import System.Random
```

次に、`randomRIO` 関数を使って指定した範囲内のランダムな数値を生成することができます。例えば、1から10の間の数値をランダムに生成するには以下のように記述します。

```Haskell
randomRIO (1,10)
```

実行すると、1から10の間のランダムな数値が表示されます。例えば `4` や `7` などが表示されるかもしれません。

## ディープダイブ

ランダムな数値を生成するために `random` モジュールがどのように動作しているか気になる方もいるかもしれません。実は、`random` モジュールは、**擬似乱数** を生成するために乱数生成アルゴリズムを使用しています。つまり、ランダムなように見える数値は実際にはアルゴリズムによって計算されたものであり、完全にランダムではありません。しかし、このような擬似乱数でも十分なランダム性を持っており、一般的に問題はありません。

## 関連リンク

- [Haskell 公式ドキュメント](https://www.haskell.org/)
- [Haskell Wiki - ランダム数生成](https://wiki.haskell.org/Random_number_generation)