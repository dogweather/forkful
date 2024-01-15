---
title:                "未来や過去の日付計算"
html_title:           "Swift: 未来や過去の日付計算"
simple_title:         "未来や過去の日付計算"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why (なぜ)

計算された日付を未来や過去に移すことに関心を持つ理由は様々です。例えば、特定のイベント日付を把握したい、タスクの期限を設定したい、あるいは取引の有効期限を計算したいなどが挙げられます。

## How To (方法)

まず、```Date```オブジェクトを作成します。それから、```Calendar```クラスを使用して、未来や過去の日付を計算することができます。以下に例を示します。

```
// カレンダーを作成
let calendar = Calendar.current

// 今日の日付を取得
let today = Date()

// 1年後の日付を計算
let oneYearFromNow = calendar.date(byAdding: .year, value: 1, to: today)!
```

このコードでは、```date(byAdding:to:)```メソッドを使用して、現在の日付から1年後の日付を計算しています。計算した日付はオプショナル型なので、```!```を使用してアンラップしています。

同様に、過去の日付を計算することもできます。例えば、5年前の日付を計算するコードは以下のようになります。

```
// 5年前の日付を計算
let fiveYearsAgo = calendar.date(byAdding: .year, value: -5, to: today)!
```

さらに、```Calendar```クラスの```dateComponents(_:from:to:)```メソッドを使用すると、指定した日付間の間隔を計算することができます。例えば、2つの日付の間の年数を計算するコードは以下のようになります。

```
// 日付を指定
let startDate = calendar.date(from: DateComponents(year: 2010, month: 1, day: 1))!
let endDate = calendar.date(from: DateComponents(year: 2020, month: 1, day: 1))!

// 年数を計算
let years = calendar.dateComponents([.year], from: startDate, to: endDate).year!
```

この例では、```startDate```と```endDate```の間の年数を計算しています。```dateComponents(_:from:to:)```メソッドは```DateComponent```型を返すので、必要な間隔だけを選択して使用します。この場合、```year```を指定しているので、結果も```year```の数値となります。

## Deep Dive (詳細)

日付の計算は、開発者にとって非常に重要な機能です。特に、取引やタスクの有効期限など、日付が重要な役割を果たす場合は、正確に計算する必要があります。

Swiftでは、```Calendar```クラスを使用して日付の計算を行うことができます。このクラスには、多くの便利なメソッドが用意されており、様々な日付計算を行うことができます。

また、日時の処理をする際には、時間帯やロケールにも注意する必要があります。```DateFormatter```クラスを使用することで、指定されたロケールや時間帯に合わせて日時を表示することができます。

## See Also (関連リンク)

- [Swift 公式ドキュメント](https://developer.apple.com/documentation/swift)
- [Date and Time Programming Guide](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/DatesAndTimes/DatesAndTimes.html#//apple_ref/doc/uid/10000039i)