---
title:                "リファクタリング"
date:                  2024-01-26T03:37:15.937062-07:00
model:                 gpt-4-0125-preview
simple_title:         "リファクタリング"
programming_language: "Swift"
category:             "Swift"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/refactoring.md"
---

{{< edit_this_page >}}

## 何となぜ？
リファクタリングとは、既存のコンピュータコードの構造を変更するプロセスであり、外部の振る舞いを変更しないことです。プログラマーは、コードベースをクリーンアップし、読みやすさ、保守性を向上させ、最小限の技術的負債で将来の機能への道を整えるためにこれを行います。

## どのようにして：
基本的なSwiftの例から始めましょう。ここでは、繰り返しコードがいくつかあります：

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("名: \(firstName)")
    print("姓: \(lastName)")
    print("年齢: \(age)")
}

func printUserJob(title: String, company: String) {
    print("職名: \(title)")
    print("会社: \(company)")
}
```

これをリファクタリングするには、ユーザー属性をカプセル化し、詳細を印刷するメソッドを追加するために`User`構造体を作成することが含まれます：

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("名: \(firstName)")
        print("姓: \(lastName)")
        print("年齢: \(age)")
        print("職名: \(jobTitle)")
        print("会社: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "ソフトウェアデベロッパー", company: "Tech Solutions")
user.printDetails()
```

### サンプル出力：
```
名: John
姓: Doe
年齢: 30
職名: ソフトウェアデベロッパー
会社: Tech Solutions
```

## ディープダイブ
リファクタリングは、ソフトウェアエンジニアリングの初期から存在するものですが、1990年代後半に特にMartin Fowlerの画期的な本「Refactoring: Improving the Design of Existing Code」を通じて広まりました。この本は、コードは別のフェーズを待つのではなく、小さなステップで継続的にクリーンアップされるべきだという原則を示しました。

手動リファクタリングの代替手段には、重複コードを検出、簡略化の提案、コードの一部を自動生成するのに役立つ自動化ツールやIDE（統合開発環境）が含まれます。Swift開発のためのXcodeは、名前の変更やメソッドの抽出機能など、さまざまなリファクタリングツールを提供しており、プロセス中の人的エラーの可能性を減らすことができます。

リファクタリングを実装する際には、堅牢なテストスイートを用意しておくことが重要です。テストは安全ネットとして機能し、行っている変更がバグを導入していないことを保証します。これは、リファクタリングの主な目的が外部の振る舞いに影響を与えずに内部構造を変更することであるため、重要です。

## 参照
- ["Refactoring: Improving the Design of Existing Code" by Martin Fowler](http://martinfowler.com/books/refactoring.html)
- [AppleによるSwiftドキュメンテーション](https://swift.org/documentation/)
- [Xcodeリファクタリングツールの使用](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Ray WenderlichのSwiftスタイルガイド](https://github.com/raywenderlich/swift-style-guide)