---
title:    "Swift: テストを書く"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/writing-tests.md"
---

{{< edit_this_page >}}

## なぜテストを書くのか

テストを書く理由はたくさんありますが、最も重要な理由はプログラムの信頼性を高めることです。テストをすることで、プログラムのバグやエラーを事前に発見し、品質の高いコードを作成することができます。

## テストの書き方

このブログでは、Swift言語でのテストの書き方について説明します。まずはテストコードを書く前に、以下のコマンドを実行し、テスト用のファイルを作成しましょう。

```Swift
touch ExampleTests.swift
```

次に、テストする対象のコードを用意し、その下にテストコードを書きます。例えば、以下のようなコードがあったとします。

```Swift
func add(_ a: Int, _ b: Int) -> Int {
    return a + b
}
```

このコードをテストするために、ExampleTests.swiftにテストコードを書きます。

```Swift
import XCTest
@testable import Example

class ExampleTests: XCTestCase {
    func testAdd() {
        // テストしたいコードを呼び出し、その結果を変数に格納する
        let result = add(3, 5)
        // 期待する結果と実際の結果が一致するかどうかを判定する
        XCTAssertEqual(result, 8)
    }
 }
```

これでテストコードの書き方は完了です。以下のコマンドを実行すると、テストが実行されます。

```Swift
xcodebuild test -scheme Example -destination 'platform=iOS Simulator,OS=latest,name=iPhone X'
```

テストが成功した場合は、以下のような結果が表示されます。

```
Test Suite 'ExampleTests' passed at <日付と時刻>. <実行時間> seconds
```

## テストの詳細

テストの書き方がわかったところで、さらに詳しくテストについて学んでみましょう。

### テストの種類

テストには大きく分けて２つの種類があります。ひとつは単体テストで、個々のコードが正しく動作するかを確かめるものです。もうひとつは結合テストで、複数のコードやコンポーネントが協調して動作するかを確かめるものです。

### テストカバレッジ

テストカバレッジはテストの範囲を示す指標です。プログラムの全行数に対してテストした行数の割合を表します。テストカバレッジは高いほど、プログラムの全体のテストに対する信頼性が高くなります。

## See Also

- [Swiftの公式ドキュメント](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Xcodeのテストガイド](https://help.apple.com/xcode/mac/current/#/devc3a1cb1cb)
- [単体テストと結合テストの違い](https://www.ibm.com/docs/ja/i/7.4?topic=test-func-apptestunit_vs_integration)
- [テストカバレッジの計算方法](https://www.tricentis.com/resources/what-is-test-coverage/)