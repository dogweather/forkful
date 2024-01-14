---
title:    "Swift: テストの書き方"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Why
テストを書くことによって、コードの品質を向上させ、将来のバグを予防することができます。

## How To
テストを書くための基本的な手順を以下に示します。

1. テストしたい関数やプロパティを定義する。

```
Swift func addNumbers(_ a: Int, _ b: Int) -> Int {
    return a + b
}
```

2. テストを行うためのテストケースを作成する。

```
Swift class TestingNumbers: XCTestCase {

    func testFunctions() {
        let sum = addNumbers(3, 4)
        XCTAssertEqual(sum, 7)
    }
}
```

3. テストを実行するためのボタンを押すか、コマンドを実行する。

```
Swift Command: xcodebuild test
```

4. テストの結果が表示されるので、テストが成功したかどうかを確認する。

```
Swift Testing started...
Test "TestingNumbers.testFunctions" passed.
```

## Deep Dive
テストを書く上で、より詳細な情報を以下にまとめました。

- テストのコードはプロダクトコードと同様、メンテナンスやリファクタリングが必要です。
- テストを書くことで、コードの不具合を早期に発見し、修正することができます。
- テストを書くことで、コードの仕様を明確に定義することができ、他の開発者がコードを理解しやすくなります。

## See Also
- [The Basics of XCTest in Swift](https://medium.com/@rolandleth/the-basics-of-xctest-in-swift-17c87c92c96d)
- [Unit Testing in Swift: A Quick Start Guide](https://learnappmaking.com/unit-testing-swift-how-to/)
- [Writing Testable Code in Swift](https://agostini.tech/2018/04/15/writing-testable-code-in-swift/)