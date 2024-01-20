---
title:                "シ answered Jun 12 'テストを書く"
html_title:           "Swift: シ answered Jun 12 'テストを書く"
simple_title:         "シ answered Jun 12 'テストを書く"
programming_language: "Swift"
category:             "Swift"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/writing-tests.md"
---

{{< edit_this_page >}}

## なに？なんで？

テストを書くとは、単純にコードの動作を確認することです。プログラマーがそれをする理由は、コードの品質を保証し、バグを見つけて修正するためです。また、チームで作業する際には、同じコードを複数人で書くことによってコードの一貫性を保つこともできます。

## 作り方：

```Swift
func add(_ a: Int, _ b: Int) -> Int {
    return a + b
}
// add 関数をテストする
assert(add(2, 2) == 4, "add 関数は正しく動作しません。")
```

```Swift
struct Person {
    let name: String
    let age: Int
    
    func greet() -> String {
        return "こんにちは、私の名前は\(name)です。\(age)歳です。"
    }
}
// Person 構造体をテストする
let person = Person(name: "太郎", age: 30)
assert(person.greet() == "こんにちは、私の名前は太郎です。30歳です。", "greet 関数は正しく動作しません。")
```

## 詳しく：

テストは、プログラミングの歴史の中で重要な役割を果たしてきました。以前は、人間が手動でコードをテストすることが一般的でしたが、現在ではプログラマーが自動化されたテストを作成することができるようになりました。代替手法としては、デバッグツールを使用したり、コードをレビューしたりすることもあります。テストを実装する際には、テストカバレッジ（コードのどの部分がテストされているか）を確認することも重要です。

## 参考にする：

- [テスト駆動開発（TDD）について](https://www.geeksforgeeks.org/test-driven-development-tdd/)
- [Swift でのユニットテストの作成](https://www.hackingwithswift.com/articles/101/how-to-write-unit-tests-in-swift)