---
title:                "ランダム数の生成"
date:                  2024-01-20T17:50:21.104535-07:00
model:                 gpt-4-1106-preview
simple_title:         "ランダム数の生成"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
ランダム数を生成するとは、予測できない数値を作り出すことです。プログラマはゲーム、シミュレーション、セキュリティ、テストデータの生成など、さまざまな理由でランダム数を使います。

## How to: (方法）
Swiftでランダム数を生成する基本的な方法の例です。

```Swift
// 整数のランダム数
let randomInt = Int.random(in: 1...100)
print(randomInt) // 例: 42

// 浮動小数点数のランダム数
let randomDouble = Double.random(in: 0..<1)
print(randomDouble) // 例: 0.843

// 真偽値のランダムな値
let randomBool = Bool.random()
print(randomBool) // 例: true
```

## Deep Dive (深く掘り下げて)
ランダム数生成の歴史は長く、コンピュータの初期から存在します。真のランダム性をコンピュータで実現するのは複雑で、多くの場合、乱数は疑似乱数発生器によって生成されます。これは、種(seed)から一連の予測できないように見える数値を生成します。Swiftでは、`arc4random()`や`random()`のような組み込み関数がこの仕事を代行します。

Swiftの`random()`は、さまざまな範囲や型に対応するための方法を提供しており、`arc4random_uniform()`を使用する代わりとなることができます。Swift 4.2以降で導入されたこのシステムは、以前よりもはるかに簡単で安全な方法でランダム数を扱えるようにしました。

## See Also (関連項目)
- Swift's official documentation on random numbers: https://developer.apple.com/documentation/swift/randomnumbergenerator
- A discussion on pseudo-random vs true random numbers: https://stackoverflow.com/questions/363681/how-do-i-generate-random-numbers-in-swift

以上がSwiftでランダム数を生成する方法と、それについてのいくつかの背景情報です。さらなる探求を促すための関連項目も合わせてご覧ください。