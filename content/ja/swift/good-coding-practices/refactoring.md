---
date: 2024-01-26 03:37:15.937062-07:00
description: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3068\u306F\u3001\u65E2\
  \u5B58\u306E\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30B3\u30FC\u30C9\u306E\u69CB\u9020\
  \u3092\u5909\u66F4\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3042\u308A\u3001\u5916\
  \u90E8\u306E\u632F\u308B\u821E\u3044\u3092\u5909\u66F4\u3057\u306A\u3044\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\
  \u30D9\u30FC\u30B9\u3092\u30AF\u30EA\u30FC\u30F3\u30A2\u30C3\u30D7\u3057\u3001\u8AAD\
  \u307F\u3084\u3059\u3055\u3001\u4FDD\u5B88\u6027\u3092\u5411\u4E0A\u3055\u305B\u3001\
  \u6700\u5C0F\u9650\u306E\u6280\u8853\u7684\u8CA0\u50B5\u3067\u5C06\u6765\u306E\u6A5F\
  \u80FD\u3078\u306E\u9053\u3092\u6574\u3048\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
lastmod: '2024-02-25T18:49:40.576217-07:00'
model: gpt-4-0125-preview
summary: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3068\u306F\u3001\u65E2\
  \u5B58\u306E\u30B3\u30F3\u30D4\u30E5\u30FC\u30BF\u30B3\u30FC\u30C9\u306E\u69CB\u9020\
  \u3092\u5909\u66F4\u3059\u308B\u30D7\u30ED\u30BB\u30B9\u3067\u3042\u308A\u3001\u5916\
  \u90E8\u306E\u632F\u308B\u821E\u3044\u3092\u5909\u66F4\u3057\u306A\u3044\u3053\u3068\
  \u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30B3\u30FC\u30C9\
  \u30D9\u30FC\u30B9\u3092\u30AF\u30EA\u30FC\u30F3\u30A2\u30C3\u30D7\u3057\u3001\u8AAD\
  \u307F\u3084\u3059\u3055\u3001\u4FDD\u5B88\u6027\u3092\u5411\u4E0A\u3055\u305B\u3001\
  \u6700\u5C0F\u9650\u306E\u6280\u8853\u7684\u8CA0\u50B5\u3067\u5C06\u6765\u306E\u6A5F\
  \u80FD\u3078\u306E\u9053\u3092\u6574\u3048\u308B\u305F\u3081\u306B\u3053\u308C\u3092\
  \u884C\u3044\u307E\u3059\u3002"
title: "\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0"
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
