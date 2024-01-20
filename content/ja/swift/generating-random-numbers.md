---
title:                "ランダムな数字の生成"
html_title:           "C#: ランダムな数字の生成"
simple_title:         "ランダムな数字の生成"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 何？そしてなぜ？

ランダムな数値生成とは、予測不可能な数値を生成するプロセスのことです。この操作は、ゲーム、シミュレーション、テストなど、偶然性やユニーク性が必要なプログラムでよく使用されます。

## どうやるか：

Swiftでランダムな数を生成する最も一般的な方法は以下の通りです:

```swift
let randomInt = Int.random(in: 1...10)
print(randomInt)
```

このコードは1から10までの間でランダムな整数を生成します。出力は 「5」、「7」、「3」など、実行するたびに変わります。

もしあなたがランダムな小数が欲しいなら、以下のように書くことが出来ます:

```swift
let randomDouble = Double.random(in: 0..<1)
print(randomDouble)
```

## ディープダイブ：

ランダム性はコンピュータサイエンスの歴史と深く結びついています。初期のコンピュータでは真のランダム性を生成するのは困難でした。現在では、Swiftでは `arc4random_uniform` のような関数を使って高品質なランダムな数を生成することができますが、それには些か複雑なバックエンドプロセスが関わっています。

ランダムな数値生成の代替方法がいくつかあります。一つには、よく確立された外部ライブラリを使用する方法があります。または、独自の乱数生成アルゴリズムを作ることもできますが、これは非推奨です。なぜなら、正しく実装するのは難しく、セキュリティ上のリスクも増大するからです。

先ほど示した `Int.random(in:)` や `Double.random(in:)` の詳細について説明します。これらのメソッドは、引数として指定した範囲内でランダムな数を生成します。特定の範囲を指定しないと、全ての可能な値が等しく選ばれる確率を持つ真のランダムさを提供します。

## 参考資料：

1. [Apple Documentation - Swift Standard Library - random(in:)](https://developer.apple.com/documentation/swift/int/2995648-random)
2. [Wikipedia - Random number generation](https://en.wikipedia.org/wiki/Random_number_generation)