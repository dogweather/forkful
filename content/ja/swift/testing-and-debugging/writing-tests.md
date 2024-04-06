---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:04.111532-07:00
description: "\u65B9\u6CD5\uFF1A Swift\u306FXCTest\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\
  \u30AF\u3092\u901A\u3058\u3066\u30C6\u30B9\u30C8\u306E\u30B5\u30DD\u30FC\u30C8\u3092\
  \u884C\u3063\u3066\u304A\u308A\u3001\u3053\u308C\u306FXcode\u306B\u7D71\u5408\u3055\
  \u308C\u3066\u3044\u307E\u3059\u3002\u305F\u3068\u3048\u3070\u30012\u3064\u306E\u6570\
  \u306E\u5408\u8A08\u3092\u8A08\u7B97\u3059\u308B\u95A2\u6570\u306A\u3069\u3001\u30B3\
  \u30FC\u30C9\u306E\u500B\u3005\u306E\u90E8\u5206\u3092\u691C\u8A3C\u3059\u308B\u305F\
  \u3081\u306E\u30E6\u30CB\u30C3\u30C8\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002"
lastmod: '2024-04-05T22:38:42.116809-06:00'
model: gpt-4-0125-preview
summary: "\u65B9\u6CD5\uFF1A Swift\u306FXCTest\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\
  \u30AF\u3092\u901A\u3058\u3066\u30C6\u30B9\u30C8\u306E\u30B5\u30DD\u30FC\u30C8\u3092\
  \u884C\u3063\u3066\u304A\u308A\u3001\u3053\u308C\u306FXcode\u306B\u7D71\u5408\u3055\
  \u308C\u3066\u3044\u307E\u3059\u3002\u305F\u3068\u3048\u3070\u30012\u3064\u306E\u6570\
  \u306E\u5408\u8A08\u3092\u8A08\u7B97\u3059\u308B\u95A2\u6570\u306A\u3069\u3001\u30B3\
  \u30FC\u30C9\u306E\u500B\u3005\u306E\u90E8\u5206\u3092\u691C\u8A3C\u3059\u308B\u305F\
  \u3081\u306E\u30E6\u30CB\u30C3\u30C8\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3053\u3068\
  \u304C\u3067\u304D\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
weight: 36
---

## 方法：
SwiftはXCTestフレームワークを通じてテストのサポートを行っており、これはXcodeに統合されています。たとえば、2つの数の合計を計算する関数など、コードの個々の部分を検証するためのユニットテストを書くことができます。

```swift
import XCTest
@testable import YourApp

class YourAppTests: XCTestCase {

    func testSum() {
        let result = Calculator().sum(a: 1, b: 2)
        XCTAssertEqual(result, 3, "合計関数は期待される値を返しませんでした。")
    }
}
```

このテストを実行するには、通常XcodeでCommand-Uを押します。Xcodeのテストナビゲーターの出力がテストが成功したか失敗したかを教えてくれます。

例えば、成功したテストの出力：
```
Test Case '-[YourAppTests testSum]' passed (0.005 seconds).
```

より高度なテストシナリオでは、より表現豊かな構文でテストを書くために、Quick/Nimbleなどのサードパーティ製ライブラリを採用することがあります。

Quick/Nimbleを使った場合の同じテストは、こんな感じに書くかもしれません：

```swift
// QuickとNimbleをSwiftパッケージマネージャーに追加するか、CocoaPods/Carthageを使ってインストールします
import Quick
import Nimble
@testable import YourApp

class CalculatorSpec: QuickSpec {
    override func spec() {
        describe("Calculator") {
            context("数を合計するとき") {
                it("正しい合計を返すべき") {
                    let calculator = Calculator()
                    expect(calculator.sum(a: 1, b: 2)).to(equal(3))
                }
            }
        }
    }
}
```

このテストを実行すると、テスト終了時の出力がテストのコンソールやCI/CDツールのログに表示され、テストが成功したか失敗したかを示します。これにより、テストと期待値を記述する形式がより読みやすくなります。
