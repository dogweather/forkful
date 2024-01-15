---
title:                "ランダムな数の生成"
html_title:           "Swift: ランダムな数の生成"
simple_title:         "ランダムな数の生成"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数値を生成することのメリットは、様々なアプリケーションで役立つことです。例えば、ゲームではランダムな要素を加えることでプレイヤーの体験をより面白くすることができます。また、データのサンプリングやテストデータの作成など、コンピュータサイエンスの分野でも重要な役割を果たします。

## 作り方

ランダムな数値を生成するには、Swiftの標準ライブラリである`arc4random_uniform`関数を使用します。この関数は、0から指定した数値までの間でランダムな整数を生成します。例えば、0から10までのランダムな数値を生成するには次のように書きます。

```Swift
let randomNumber = arc4random_uniform(11)
print(randomNumber) // Output: 9
```

また、この関数を使用する際には、ランダムな数値が欲しい変数の型に合わせてキャストすることを忘れないようにしましょう。例えば、0から10までのランダムな整数をDouble型の変数に代入するには、次のように書きます。

```Swift
let randomNumber = Double(arc4random_uniform(11))
print(randomNumber) // Output: 9.0
```

## ディープダイブ

ランダムな数値を生成するためにSwiftのarc4random_uniform関数を使用することが一般的ですが、実際には乱数生成アルゴリズムの一種であるメルセンヌ・ツイスターを使用しています。このアルゴリズムは、よりランダムな数値を生成するために計算力を多く必要としますが、より高品質なランダム数値を提供します。

また、ランダムな数値を生成する際にはシード値を指定することで、同じ乱数を生成することができます。例えば、ゲームの開始時に同じ乱数を使用することで、プレイヤーが同じスタート地点からゲームを開始することができます。

## 同様に参考になるもの

- <https://developer.apple.com/documentation/swift/int/2884963-arc4random_uniform>：Swiftの公式ドキュメント
- <https://www.raywenderlich.com/146946/arc4random-and-arc4random_uniform>：Raywenderlichのチュートリアル記事
- <https://ja.wikipedia.org/wiki/%E3%83%A1%E3%83%AB%E3%82%BB%E3%83%B3%E3%83%8C%E3%83%BB%E3%83%84%E3%82%A4%E3%82%B9%E3%82%BF%E3%83%BC>：メルセンヌ・ツイスターのウィキペディア記事