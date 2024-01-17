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

## 今、何の話？
現在の日付を取得するとは、プログラマーが現在の日付を調べることです。プログラマーは、日付を取得することで、日付を処理する際の基準となる重要な情報を得ることができます。

## 方法：
```Swift
// 日付を取得する
let currentDate = Date() 
// フォーマットを指定して日付を文字列に変換する
let formatter = DateFormatter()
formatter.dateFormat = "yyyy/MM/dd"
let dateString = formatter.string(from: currentDate)
print(dateString) //出力：2021/02/15
```

## さらに深く掘り下げる：
日付を取得するには、Dateというデータ型を使用します。Date型は、1970年1月1日からの秒数を表す整数型で、現在のタイムゾーンでの現在の日付と時刻を表します。

また、DateFormatterを使用することで、日付を指定したフォーマットに変換することができます。例えば、"yyyy/MM/dd"というフォーマットを指定することで、現在の日付を"2021/02/15"のような形式で取得することができます。

他にも、CalendarやNSDateなどのオブジェクトを使用することで、より詳細な日付情報を取得することができます。

## 関連リンク：
- [AppleのDateドキュメント](https://developer.apple.com/documentation/foundation/date)
- [AppleのDateFormatterドキュメント](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swiftでは日付をどのように処理するか](https://techlife.cookpad.com/entry/2019/11/13/090000)