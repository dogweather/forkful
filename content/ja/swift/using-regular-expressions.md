---
title:    "Swift: 正規表現の使用"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## なぜ使うのか
多くのプログラミング言語で利用できる正規表現。それは、テキストデータを効率的に処理するための強力なツールです。Swiftでも同様に、複雑なテキストデータの処理が簡単になります。さあ、正規表現を使ってコーディングを効率化しましょう！

## 使い方
正規表現は、特定のパターンに一致するテキストを検索したり、置換したりするための表現方法です。例えば、テキスト内のメールアドレスや電話番号を一度に検索したり、置き換えたりすることができます。

```Swift
let emailRegex = #"([A-Z0-9._%+-]+@[A-Z0-9.-]+\.[A-Z]{2,})"#
let phoneRegex = #"(0\d{1,4}-\d{2,4}-\d{3,4})"# 

let text = "こんにちは！ 私のメールアドレスはexample@example.comです。電話番号は053-123-4567です。"

let emailMatches = text.matches(for: emailRegex)
print(emailMatches) // ["example@example.com"]

let phoneNumbers = text.replacingOccurrences(of: phoneRegex, with: "PHONE NUMBER") 
print(phoneNumbers) // "こんにちは！ 私のメールアドレスはexample@example.comです。電話番号はPHONE NUMBERです。"
```

## もっと詳しく
正規表現では、パターン内に特殊な記号を使うことで、より柔軟な検索や置換が可能です。例えば、`[]`を使うことで特定の文字の範囲を指定したり、`{}`を使うことで文字の繰り返し数を指定したりすることができます。また、グループ化や後方参照を使うことで、マッチした部分を取得したり、置換文で再利用することができます。

大文字小文字の区別や、正規表現のオプションを指定することもできます。詳しくは、正規表現のドキュメントを参照してください。

## 参考文献
- [Swift正規表現入門](https://qiita.com/satisfactory/items/a3fbc3efb1a7462422a3)
- [正規表現の基礎](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Predicates/Articles/pSyntax.html#//apple_ref/doc/uid/TP40001794-CJADHBFH)
- [Swift API Reference: NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)

## 関連リンク
- [正規表現チートシート](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [正規表現のサンプル](https://regex101.com/)
- [Swiftプログラミング入門](https://developer.apple.com/swift/)