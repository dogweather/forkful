---
title:    "Swift: 将来または過去の日付を計算する"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付を未来や過去に計算する必要性は、アプリケーションでイベントを管理する場合など、時にはとても重要です。Swiftでは、特定の日付を基準にして未来や過去の日付を計算することができます。

## 方法

まず、計算したい日付のコンポーネントを取得する必要があります。下記のような例を見てみましょう。

```Swift
// 今日の日付を取得
let today = Date()
// カレンダーを取得
let calendar = Calendar.current
// 未来の日付を計算 (1年後)
let oneYearFromToday = calendar.date(byAdding: .year, value: 1, to: today)
// 過去の日付を計算 (1ヶ月前)
let oneMonthAgo = calendar.date(byAdding: .month, value: -1, to: today) 

print(oneYearFromToday) // 2021-09-09 05:36:29 +0000
print(oneMonthAgo) // 2020-07-09 05:36:29 +0000
```

ここでは、日付を取得するために `Date()` という関数を使用し、カレンダーを使って任意の日数を追加または減じることで未来や過去の日付を計算しています。`.year` や `.month` など、カレンダーのコンポーネントと組み合わせることで、さまざまな日付を計算することができます。

## ディープダイブ

Swiftで日付を計算する時、さまざまな方法があります。この記事では、カレンダーを使用する方法を紹介しましたが、 `DateComponents` や `DateInterval` などの方法もあります。また、タイムゾーンやロケールを考慮することも重要です。より詳細に学びたい方は、Swiftの公式ドキュメントやオンラインのチュートリアルなどを参考にしてみてください。

## さらに見る

[Swiftでの日付計算の方法](https://developer.apple.com/documentation/foundation/datecomponents)

[カレンダーを使用した日付計算のチュートリアル](https://ios-diploma.com/blog/prog-nastroika/ios/datecalendariandcaleanddate)