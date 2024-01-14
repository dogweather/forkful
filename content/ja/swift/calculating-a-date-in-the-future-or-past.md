---
title:                "Swift: 未来または過去の日付の計算"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付の計算を行う理由はさまざまです。例えば、将来の誕生日や記念日を知るために、またはプロジェクトの締切日を把握するために、日付の計算が必要になることがあります。Swiftを使えば、簡単に日付を計算することが可能です。

## 方法

### 日付を増やす

日付を増やす場合には、DateComponentsを使用します。たとえば、今日の日付から2ヶ月後の日付を計算するには、次のようにコードを書きます。

```Swift
//今日の日付を取得
let today = Date()
//コンポーネントを作成
var dateComponents = DateComponents()
//2ヶ月後を設定
dateComponents.month = 2
//日付を増やす
let futureDate = Calendar.current.date(byAdding: dateComponents, to: today)
//結果を表示
print(futureDate)
```

このコードを実行すると、今日の日付から2ヶ月後の日付が計算されて表示されます。

### 日付を減らす

日付を減らす場合には、同じくDateComponentsを使用します。たとえば、今日の日付から1週間前の日付を計算するには、次のようにコードを書きます。

```Swift
//今日の日付を取得
let today = Date()
//コンポーネントを作成
var dateComponents = DateComponents()
//1週間前を設定
dateComponents.week = -1
//日付を減らす
let pastDate = Calendar.current.date(byAdding: dateComponents, to: today)
//結果を表示
print(pastDate)
```

このコードを実行すると、今日の日付から1週間前の日付が計算されて表示されます。

## ディープダイブ

日付の計算には、DateComponentsのほかにもCalendarを使用することができます。また、異なるタイムゾーンでの日付の計算や、平日の計算なども可能です。詳細については公式ドキュメントを参照してください。

## 他にも見てみる

- [Date and Time Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html#//apple_ref/doc/uid/10000039i)
- [Working with Dates and Times in Swift](https://www.swiftbysundell.com/basics/dates-and-times/)
- [日付を計算する方法 - Swift編](https://qiita.com/yokoyazo/items/a73f803fccb3c5a64acb)