---
title:                "「文字列から日付を解析する」"
html_title:           "Swift: 「文字列から日付を解析する」"
simple_title:         "「文字列から日付を解析する」"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何をしているの？
日付を文字列から解析することは、プログラマーが日付を特定の形式に変換することを意味します。このプロセスは、データを正確に理解し、適切に処理するために重要です。

## 方法：
Swiftで日付を文字列から解析するには、DateFormatterクラスとdate(from: string)メソッドを使用します。下記の例を参考にしてください。

```Swift
let dateFormatter = DateFormatter() // DateFormatterクラスのインスタンスを作成
dateFormatter.dateFormat = "yyyy/MM/dd" // 日付の形式を指定
let date = dateFormatter.date(from: "2020/12/25") // "2020/12/25"を日付に変換
print(date) // 2020-12-25 00:00:00 +0000 と出力される
```

## 詳細情報：
日付を文字列から解析するには、様々な方法があります。その中でも、NSDateFormatterを使用する方法は一般的です。他にもFoundationフレームワークのNSCalendarや、サードパーティーライブラリのChronoなどもあります。また、日付の形式やロケールによっても挙動が異なるので、注意が必要です。

## 関連情報：
以下のリンクから詳細なドキュメントを参照できます。
- [NSDateFormatter Class Reference](https://developer.apple.com/documentation/foundation/nsdateformatter)
- [NSCalendar Class Reference](https://developer.apple.com/documentation/foundation/nscalendar)
- [Chrono Library](https://github.com/chronotope/Chrono)