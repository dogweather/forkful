---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/haskell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何となぜ？

乱数生成は、あらゆる数字の中から予測不可能な数を選び出すことです。プログラム中で乱数を利用する主な理由は、意図しないエラーを引き起こす可能性がある状況をシミュレーションしたり、ゲームのAIをより対戦相手に予測できないようにするためです。

## うまくいく方法：

Haskellで乱数を生成する基本的なコードは以下の通りです。

```Haskell
import System.Random
main = do 
  g <- newStdGen
  print (take 5 (randomRs (1,100) g))
```

これを実行すると、1から100までの範囲でランダムに選ばれた5つの数を得られます。

## 詳しく見てみましょう：

Haskellの`System.Random`モジュールは、擬似乱数生成器（PRNG）を提供しています。このPRNGは、1946年にジョン・フォン・ノイマンによって初めて開発され、再現可能性と予測不可能性をバランス良く持つことで知られています。

Haskellには他にも乱数生成のためのライブラリ、例えばQuantumRandomやpcg-randomなどが存在します。それぞれが異なるアルゴリズムを用いて乱数を生成します。

また、Haskellの乱数生成はIOアクションを使用します。これは、副作用のある操作を行うためのHaskellの方法です。ランダムな数を生成することは、走行中のプログラム外の世界から情報を取り出す副作用を持つため、IOアクションに分類されます。

## さらなる情報:

関連する情報源や更なる学習のためのリンク：

- Haskellの公式ドキュメンテーションの`System.Random`モジュール：http://hackage.haskell.org/package/random-1.1/docs/System-Random.html
- QuantumRandomライブラリの公式ドキュメンテーション：https://hackage.haskell.org/package/QuantumRandom
- pcg-randomライブラリの公式ドキュメンテーション：https://hackage.haskell.org/package/pcg-random