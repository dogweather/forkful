---
title:                "日付を文字列に変換する"
html_title:           "Swift: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## 何 & なぜ?
日付を文字列に変換するとは、プログラマーが日付の情報をコンピュータで扱いやすくするための作業です。プログラマーは、日付を文字列に変換することで、例えばデータベースやフロントエンドのフォームなどで使用することができます。

## どのように:
```Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd"
let dateString = dateFormatter.string(from: date)
print(dateString)
```
出力: "2021/09/07"

## 深堀り:
日付を文字列に変換するために、NSDateFormatterクラスを使用します。これは、文字列と日付の相互変換を行うためのユーティリティクラスです。適切な日付フォーマットを設定することで、日付を好きな形式の文字列に変換することができます。もし、日付の情報を扱わない場合は、DateFormatterを使用する必要はありません。代わりに、StringInterpolationを使用して、文字列の中に日付の情報を埋め込むこともできます。

## 関連リンク:
- [Apple Developer Documentation - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift by Sundell - Formatting dates in Swift](https://www.swiftbysundell.com/articles/formatting-dates-in-swift/)