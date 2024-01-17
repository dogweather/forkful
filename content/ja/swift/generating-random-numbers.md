---
title:                "ランダムナンバーの生成"
html_title:           "Swift: ランダムナンバーの生成"
simple_title:         "ランダムナンバーの生成"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
乱数生成とは、プログラマーがコード内でランダムな数値を作成することです。プログラマーは、この機能を使用して、ランダム性が必要なシミュレーションやゲームを作成します。

## 作り方：
乱数を生成するには、Swiftの標準ライブラリで利用可能なランダム関数を使用します。以下は、円周率の近似値を生成するコード例です。

```Swift
let randomValue = Float.random(in: 3.0...4.0)
let piValue = randomValue * randomValue
```

実行すると、毎回異なる近似値が生成されます。例えば、一度目は3.697524、二度目は3.323656といった具合にです。

## 詳細：
乱数生成の歴史的背景には、計算機における非決定論的アルゴリズムの研究があります。代替手段として、プログラマーは外部の乱数生成器APIを使用することもできます。また、乱数生成はハードウェアによって実装されることもあります。

## 関連情報：
乱数生成についてもっと詳しく学ぶには、以下のリンクを参考にしてください。

- [Appleの公式ドキュメント](https://developer.apple.com/documentation/swift/swift_standard_library/random)
- [乱数生成に関する記事](https://qiita.com/stellalee23/items/fe8fd3c11dd033d54d39)
- [疑似乱数生成アルゴリズムについての解説](https://vivivai.net/sar/baseline/random/)