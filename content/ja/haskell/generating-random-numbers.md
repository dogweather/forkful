---
title:                "Haskell: 「ランダムな数値を生成する」"
simple_title:         "「ランダムな数値を生成する」"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数値を生成することの利点について説明します。Haskellでランダムな数値を生成することで、ゲームやシミュレーション、ランダムな指示を必要とするアルゴリズムなどに使用することができます。

## 作り方

まず、`System.Random`モジュールをインポートする必要があります。その後、`randomRIO`関数を使用して、特定の範囲のランダムな数値を生成します。例えば、`1`から`10`の間のランダムな数値を生成するには、以下のようになります。

```Haskell
import System.Random

main = do
  num <- randomRIO (1, 10)
  print num
```

上記のコードを実行すると、`1`から`10`の間のランダムな数値が表示されます。

## 深堀り

ランダムな数値を生成する際、実際には乱数ジェネレーターと呼ばれるアルゴリズムが用いられています。このアルゴリズムは、シードと呼ばれる入力値に基づいてランダムな数値を生成します。そのため、同じシードを与えると同じ数値が生成されます。Haskellの`System.Random`モジュールでは、デフォルトでシステム時刻を使用してシードを生成するため、実行毎に異なる数値が生成されます。

## 参考リンク

- [Haskellのランダムな数値生成チュートリアル（英語）](https://wiki.haskell.org/Random_numbers)
- [System.Randomモジュールのドキュメンテーション（英語）](https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html)
- [乱数ジェネレーターについての詳細な説明（日本語）](https://ja.wikipedia.org/wiki/%E4%B9%B1%E6%95%B0%E3%82%B8%E3%82%A7%E3%83%8D%E3%83%AC%E3%83%BC%E3%82%BF)
- [Haskellのシステム時刻を使用した乱数生成の仕組みについての詳細な説明（英語）](https://stackoverflow.com/questions/11697241/how-to-use-random-number-in-haskell)