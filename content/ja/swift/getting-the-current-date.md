---
title:                "現在の日付を取得する"
html_title:           "Swift: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ今日の日付を取得するのか

日付を取得することは、多くのアプリケーションにとって重要な機能です。例えば、スケジュール管理や登録したデータを時間順に表示する際に必要になります。Swiftでは、現在の日付を簡単に取得することができます。

## ‎‎取得方法

```Swift
let date = Date() // 現在の日付を取得
let dateFormatter = DateFormatter() // DateFormatterを使って日付をフォーマット
dateFormatter.dateFormat = "yyyy-MM-dd" // 使用するフォーマットを指定
let stringDate = dateFormatter.string(from: date) // 日付を文字列に変換
print(stringDate) // 出力結果： 2021-04-20
```

上のコード例では、`Date()`を使って現在の日付を取得し、`DateFormatter`を使って指定したフォーマットに合わせて日付を表示しています。日付を文字列に変換することで、アプリケーション内で便利に使用することができます。

## 深堀り

日付を取得する方法には、他にも様々なオプションがあります。例えば、タイムゾーンを指定したり、24時間制ではなく12時間制で表示することも可能です。また、日付を比較したり、計算したりすることもできます。詳しくは[公式ドキュメント](https://developer.apple.com/documentation/foundation/date)を参照してください。

## ‎‎See Also

- [How to Format Dates in Swift](https://www.hackingwithswift.com/example-code/system/how-to-format-dates-with-dateformatter)
- [Date and Time Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html#//apple_ref/doc/uid/10000039i)