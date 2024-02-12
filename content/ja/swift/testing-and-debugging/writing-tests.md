---
title:                "テストの作成"
aliases:
- /ja/swift/writing-tests.md
date:                  2024-02-03T19:32:04.111532-07:00
model:                 gpt-4-0125-preview
simple_title:         "テストの作成"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
