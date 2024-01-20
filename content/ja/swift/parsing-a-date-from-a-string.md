---
title:                "文字列から日付を解析する"
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列から日付を解析することは、文字列の形式の日付を特定の日付型に変換することです。これはデータの一貫性を保つため、プログラマーが頻繁に行います。

## 実装方法：
以下にSwiftで日付の解析の例を示します:

``` Swift
let dateString = "2022-12-13"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let date = dateFormatter.date(from: dateString)
print(date)
```

これで出力は 2022-12-13 00:00:00 +0000 のようになります。`yyyy`、 `MM`、および `dd` はそれぞれ年、月、日を表します。

## 詳細:
日付の解析はソフトウェアプログラミングの初期から続いています。しかしSwiftにおける実装は他の言語と比較して非常に簡単です。代替手段として時間スタンプの使用がありますが、それは可読性の点で不利です。内部的には、`dateFormatter.date(from: dateString)`は内部的にCocoaが提供する`NSDate`オブジェクトを使用して日付を生成します。

## 関連資料:
日付と時間のより詳しい解析については以下のリンクを参照してください:
1. 英語の公式ドキュメント: [NSDate - Foundation |Apple Developer Documentation](https://developer.apple.com/documentation/foundation/nsdate)
2. Swiftの日付の解析に関する詳細なガイド: [Working with Dates and Times in Swift](https://www.raywenderlich.com/5817-working-with-dates-and-times-in-swift)
3. 日付と時間のフォーマットの詳細: [Date and Time Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DataFormatting/Articles/dfDateFormatting10_4.html)