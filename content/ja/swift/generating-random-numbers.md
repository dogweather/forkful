---
title:                "Swift: ランダムな数字の生成"
programming_language: "Swift"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ？

ランダムな数字を生成することの *なぜ* について説明します。

ランダムな数字を使用することで、アプリやゲームなどの様々な場面で楽しさや多様性を生み出すことができます。また、乱数を生成することで、データの調査やテストを行うことも可能です。

## 方法

```Swift
let randomInt = Int.random(in: 0..<10)
print(randomInt)
```

上記のようなコードを使用して、0から9までのランダムな整数を生成することができます。このように、`random(in: ...)`を使用することで、指定した範囲のランダムな値を生成することができます。

```Swift
let randomDouble = Double.random(in: 0..<1)
print(randomDouble)
```

また、`Double`や`Float`などのように、特定のデータ型に対応するランダムな値を生成することもできます。

## 深堀り

Swiftでは、`random(in: ...)`を使用することでランダムな値を生成することができますが、実際にはどのように動いているのでしょうか？

Swiftのランダムな値生成アルゴリズムは、メルセンヌ・ツイスター(Mersenne Twister)と呼ばれるアルゴリズムを使用しています。このアルゴリズムは長い周期を持つことで、様々な目的に使用されることができます。

さらに、`randomElement()`や`shuffle()`などのメソッドを使用することで、配列からランダムな要素を取得したり、配列の要素をランダムにシャッフルすることもできます。

## はじめてのSwiftプログラミング

今回はSwiftで数学的なランダムな数値を生成する方法について学びました。ランダムな値を使用して、より楽しいアプリやゲームを作ることができます。ぜひ、実際にコーディングしてみてください。

## See Also
- [Swift Documentation on Random Numbers](https://docs.swift.org/swift-book/LanguageGuide/TheBasics.html#ID330)
- [An Introduction to Random Number Generators in Swift](https://www.raywenderlich.com/1469-an-introduction-to-random-number-generators-in-swift)
- [Understanding Random Numbers in Swift](https://www.hackingwithswift.com/example-code/generation/understanding-random-numbers-in-swift)