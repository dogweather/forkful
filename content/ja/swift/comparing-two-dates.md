---
title:                "Swift: 「2つの日付を比較する」"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# なぜ日付を比較するのか

日付を比較することは、Swiftプログラミングで非常によく使用される機能です。例えば、ある日付が特定の範囲内にあるかどうかを確認するためや、日付の並び替えをするためによく用いられます。日付を比較することで、アプリケーションのロジックを効率的に実装することができます。

## 方法

日付を比較するためには、Date型の `compare` メソッドを使用します。このメソッドは、2つの日付を比較し、 `ComparisonResult` 型の結果を返します。 `ComparisonResult` 型には、 `orderedAscending`、 `orderedDescending`、 `orderedSame` の3つのケースがあります。

```Swift
let date1 = Date() // 現在の日付を取得
let date2 = Date().addingTimeInterval(60 * 60 * 24) // 現在の日付から1日後の日付を取得
let result = date1.compare(date2) // 日付を比較して、結果を `orderedAscending` に格納
if result == .orderedAscending {
    print("date2 is a later date than date1.")
}
```

上記のコードでは、 `date2` の方が `date1` よりも後の日付であることを確認しています。このように、日付を比較することで、日付の大小を判定することができます。

## ディープダイブ

日付を比較する際に気をつけるべきことがあります。それは、日付の比較はタイムゾーンに依存するということです。つまり、日付を比較する前に、日付のタイムゾーンを正しく設定する必要があります。そうでないと、意図しない結果が返ってくる可能性があります。

また、日付を文字列に変換する場合にも、フォーマットには注意が必要です。フォーマットによって、日付の比較結果が変わることがあります。

## 参考リンク

- [Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Swift Docs - `compare(_:)` method](https://docs.swift.org/swift-book/LanguageGuide/Methods.html)
- [Swift Docs - `ComparisonResult` type](https://developer.apple.com/documentation/foundation/comparisonresult)
- [Swift Docs - Working with Dates](https://docs.swift.org/swift-book/LanguageGuide/WorkingWithDates.html)

# さらに学ぼう

日付の比較については、さらに学ぶことができます。例えば、日付の加算や差分の計算、日付のフォーマットなど、さまざまな機能があります。日付を扱うアプリケーションを開発する際には、日付の比較に加えてこれらの機能も学ぶことをおすすめします。

# 参照

[Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date) <br>
[Swift Docs - `compare(_:)` method](https://docs.swift.org/swift-book/LanguageGuide/Methods.html) <br>
[Swift Docs - `ComparisonResult` type](https://developer.apple.com/documentation/foundation/comparisonresult) <br>
[Swift Docs - Working with Dates](https://docs.swift.org/swift-book/LanguageGuide/WorkingWithDates.html)