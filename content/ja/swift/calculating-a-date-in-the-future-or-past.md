---
title:                "未来または過去の日付の計算"
html_title:           "Swift: 未来または過去の日付の計算"
simple_title:         "未来または過去の日付の計算"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何か & なぜ?
計算すると、未来や過去の日付がわかります。プログラマーがそれを行うのは、アプリケーションの予定や期限を管理するためです。

## 方法:
```Swift
// 今日の日付を生成します
let today = Date()

// 1日後を計算します
let tomorrow = Calendar.current.date(byAdding: .day, value: 1, to: today)

// 10日前を計算します
let tenDaysAgo = Calendar.current.date(byAdding: .day, value: -10, to: today)

// 「年」「月」「日」など、異なる単位で計算も可能です。
// 300日後を計算します
let threeHundredDaysLater = Calendar.current.date(byAdding: .day, value: 300, to: today)

// 結果の日付をフォーマットして出力します
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy年MM月dd日"
print(dateFormatter.string(from: tomorrow!))
// 出力: 2021年07月06日
print(dateFormatter.string(from: tenDaysAgo!))
// 出力: 2021年06月26日
```

## 深堀り:
計算する日付の単位や形式に応じて、異なる方法があります。また、現在の日付だけでなく、任意の日付から計算することも可能です。

参考文献:
[Apple Developer Documentation - Calculating Dates and Times by Incrementing](https://developer.apple.com/documentation/foundation/calendar/calculating_dates_and_times_by_incrementing)
[How to: Work With Dates and Times in Swift](https://www.raywenderlich.com/4560801-how-to-work-with-dates-and-times-in-swift)

## 関連情報:
[Stack Overflow - Swiftを使用して未来の日付を作成する方法](https://stackoverflow.com/questions/38488573/how-to-create-a-future-datetime-with-swift)