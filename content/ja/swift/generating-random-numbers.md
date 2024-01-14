---
title:    "Swift: ランダム数値の生成"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することに取り組む理由は、多くの場合、乱数が必要な特定のアプリケーションやゲームを作成する必要があるためです。幅広いランダムな数値を生成することで、より動的で面白いアプリケーションを作成することができます。

## 生成する方法

ランダムな数値を生成するためにSwiftで使用できる2つの方法があります。一つは、`arc4random()`を使用する方法です。コードの例を示します。

```Swift
let randomNumber = arc4random()
print(randomNumber)
```

このコードは、0から最大のランダムな数値までの数値を生成します。また別の方法として、`arc4random_uniform()`があります。これは、生成したいランダムな数値の範囲を指定できます。例えば、1から10までの数値を生成する場合、以下のようなコードを使用できます。

```Swift
let randomNumber = arc4random_uniform(10) + 1
print(randomNumber)
```

このコードは、1から10までの数値をランダムに生成します。どちらの方法を選んでも、確実にランダムな数値を生成することができます。

## ディープダイブ

ランダムな数字を生成する方法には、さまざまな方法があります。しかし、最も一般的な方法は、疑似乱数ジェネレーターを使用して数値を生成することです。これらのジェネレーターは、本当のランダム性をシミュレートすることで、ランダムな数値を生成します。Swiftでは、`arc4random()`や`arc4random_uniform()`などの組み込みの関数を使用して、これらのジェネレーターを実装します。

## 併せて参照

- [Swift公式ドキュメント](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID334)
- [ランダムな数値を生成する方法](https://dzone.com/articles/how-to-generate-random-numbers-in-swift)