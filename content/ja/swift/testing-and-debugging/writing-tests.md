---
aliases:
- /ja/swift/writing-tests/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:04.111532-07:00
description: "Swift\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3068\u3044\u3046\u3053\
  \u3068\u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u306E\u4ED6\
  \u306E\u30B3\u30FC\u30C9\u30E6\u30CB\u30C3\u30C8\u306E\u6B63\u3057\u3055\u3092\u691C\
  \u8A3C\u3059\u308B\u305F\u3081\u306E\u30B3\u30FC\u30C9\u3092\u4F5C\u6210\u3057\u3001\
  \u5B9F\u884C\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\
  \u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4FE1\u983C\u6027\u3092\u78BA\u4FDD\u3057\
  \u3001\u958B\u767A\u30B5\u30A4\u30AF\u30EB\u306E\u65E9\u3044\u6BB5\u968E\u3067\u30D0\
  \u30B0\u3092\u767A\u898B\u3057\u3001\u610F\u56F3\u3057\u306A\u3044\u7D50\u679C\u306A\
  \u3057\u306B\u5C06\u6765\u7684\u306A\u30B3\u30FC\u30C9\u306E\u30EA\u30D5\u30A1\u30AF\
  \u30BF\u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3059\u308B\u305F\u3081\u306B\u3053\
  \u308C\u3092\u884C\u3044\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.232649
model: gpt-4-0125-preview
summary: "Swift\u3067\u30C6\u30B9\u30C8\u3092\u66F8\u304F\u3068\u3044\u3046\u3053\u3068\
  \u306F\u3001\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u5185\u306E\u4ED6\u306E\
  \u30B3\u30FC\u30C9\u30E6\u30CB\u30C3\u30C8\u306E\u6B63\u3057\u3055\u3092\u691C\u8A3C\
  \u3059\u308B\u305F\u3081\u306E\u30B3\u30FC\u30C9\u3092\u4F5C\u6210\u3057\u3001\u5B9F\
  \u884C\u3059\u308B\u3053\u3068\u3092\u610F\u5473\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u4FE1\u983C\u6027\u3092\u78BA\u4FDD\u3057\u3001\
  \u958B\u767A\u30B5\u30A4\u30AF\u30EB\u306E\u65E9\u3044\u6BB5\u968E\u3067\u30D0\u30B0\
  \u3092\u767A\u898B\u3057\u3001\u610F\u56F3\u3057\u306A\u3044\u7D50\u679C\u306A\u3057\
  \u306B\u5C06\u6765\u7684\u306A\u30B3\u30FC\u30C9\u306E\u30EA\u30D5\u30A1\u30AF\u30BF\
  \u30EA\u30F3\u30B0\u3092\u5BB9\u6613\u306B\u3059\u308B\u305F\u3081\u306B\u3053\u308C\
  \u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u30C6\u30B9\u30C8\u306E\u4F5C\u6210"
---

{{< edit_this_page >}}

## 何となぜ？
Swiftでテストを書くということは、アプリケーション内の他のコードユニットの正しさを検証するためのコードを作成し、実行することを意味します。プログラマーは、信頼性を確保し、開発サイクルの早い段階でバグを発見し、意図しない結果なしに将来的なコードのリファクタリングを容易にするためにこれを行います。

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
