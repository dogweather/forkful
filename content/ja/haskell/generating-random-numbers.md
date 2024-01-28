---
title:                "乱数の生成"
date:                  2024-01-27T20:34:24.137940-07:00
model:                 gpt-4-0125-preview
simple_title:         "乱数の生成"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

Haskellで乱数を生成するとは、人間の基準では予測不可能な数値を作り出すことを意味します。これは、暗号アプリケーションから、現実世界の現象を正確にモデル化するために偶然の要素が必要なシミュレーションに至るまでのシナリオで重要です。

## 方法：

Haskellで乱数を生成するには、通常、Haskellプラットフォームの一部である`random`パッケージを使用します。ここにステップバイステップのガイドがあります：

まず、`random`パッケージがインストールされていることを確認してください。もしまだなら、CabalやStackを通じて入手できます。

### 乱数の生成

単純な乱数を生成するには、指定された範囲内でランダムな値を生成する`randomRIO`関数を使用できます。

```Haskell
import System.Random (randomRIO)

main :: IO ()
main = do
  randomNumber <- randomRIO (1, 10) :: IO Int
  putStrLn $ "Random number: " ++ show randomNumber
```

### 乱数のリスト生成

乱数のリストを生成することは少し複雑ですが、それでも直感的です：

```Haskell
import System.Random (randomRIO)

randomList :: Int -> IO [Int]
randomList 0 = return []
randomList n = do
  r <- randomRIO (1, 100)
  rs <- randomList (n-1)
  return (r:rs)

main :: IO ()
main = do
  numbers <- randomList 5
  print numbers
```

このコードスニペットは、ランダムな整数のリストを生成する`randomList`関数を作成します。望ましい範囲で`(1, 100)`を置き換えてください。

## ディープダイブ

Haskellの`random`パッケージは疑似ランダム数生成器（PRNG）を提供しており、生成される数値が真にランダムではないものの、多くのアプリケーションにおいてランダムであるように見えることを意味します。Haskellの乱数生成能力の核心は、ランダム数を生成する異なる方法を抽象化する`RandomGen`型クラスと、ランダムに生成できる型を含む`Random`型クラスにあります。

歴史的に、Haskellにおける乱数生成のアプローチは純粋性と再現性を重視してきました。これが、乱数を扱う操作が明示的に`IO`モナドで操作されたり、ジェネレータの状態を手動で渡して更新する必要がある理由です — 参照透過性を維持するために。

特定のアプリケーション、例えば暗号化では、デフォルトのPRNGによって生成される疑似ランダム番号は十分に安全でない場合があります。これらの使用例において、Haskellプログラマーはしばしば、暗号化アプリケーションの厳格な要件を満たすように設計された`crypto-random`のようなより専門的なライブラリに頼ります。

さらに、`mwc-random`のような代替ライブラリは、メルセンヌ・ツイスターなどの現代的なアルゴリズムを実装することで、シミュレーションや他のアプリケーションのための乱数の品質と性能を向上させます。

Haskellで乱数生成アプローチを選択する際には、乱数の品質、性能、およびセキュリティに関するアプリケーションのニーズを検討し、最も適切なツールやライブラリを選択することが重要です。
