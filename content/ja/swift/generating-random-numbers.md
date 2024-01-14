---
title:                "Swift: ランダムな数を生成する"
simple_title:         "ランダムな数を生成する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/generating-random-numbers.md"
---

{{< edit_this_page >}}

## なぜ

ランダムな数字を生成することの意義は、アプリやゲームでのバラエティやユーザーの興味を引くために活用することができます。また、デバッグやテストでの使用もできます。

## 生成方法

ランダムな数字を生成するには、`arc4random_uniform()`という関数を使用します。以下のコードを使用することで、0から10までのランダムな整数が生成されます。

```Swift
let randomNumber = arc4random_uniform(10)
print(randomNumber)
```
出力例:
```
5
```

ランダムな小数を生成する場合は、`Double`型を使用します。

```Swift
let randomDouble = Double(arc4random())/Double(UINT32_MAX)
print(randomDouble)
```
出力例: 
```
0.234567891
```

## ディープダイブ

ランダムな数字を生成する際には、シード値というデータを設定することもできます。これにより、同じシード値を使用すれば常に同じランダムな数字が生成されるため、アプリやゲームの再現性を確保することができます。

また、Swiftには`arc4random()`以外にも`arc4random_uniform()`, `arc4random_stir()`などの関数があります。それぞれ異なる方法でランダムな数字を生成することができるため、適切な関数を選択することが重要です。

## 参考リンク

http://iosdeveloperzone.com/2014/10/14/generating-random-numbers-in-swift/
https://developer.apple.com/documentation/swift/int/2995597-arc4random_uniform
https://developer.apple.com/documentation/swift/int/2998886-arc4random_stir
https://developer.apple.com/library/archive/documentation/Cocoa/Reference/Foundation/Miscellaneous/Foundation_Functions/index.html#//apple_ref/c/func/arc4random