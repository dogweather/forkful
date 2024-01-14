---
title:    "Swift: 現在の日付の取得"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ
今日の日付を取得する理由について1-2文で説明する。

プログラムを書いている際に、現在の日付が必要になることがあります。例えば、ログを作成する際や、タスクの期限を設定する際には、現在の日付が必要になります。SwiftのDateクラスを使用することで、簡単に現在の日付を取得することができます。

## 使い方

まずは、Dateクラスをインスタンス化します。

```
let currentDate = Date()
```

次に、Dateクラスのインスタンスから、必要な情報を取得することができます。例えば、現在の年、月、日を取得するには、次のようにします。

```
let calendar = Calendar.current
let year = calendar.component(.year, from: currentDate)
let month = calendar.component(.month, from: currentDate)
let day = calendar.component(.day, from: currentDate)
```

また、曜日や時間、分、秒などを取得することもできます。

```
let weekday = calendar.component(.weekday, from: currentDate)
let hour = calendar.component(.hour, from: currentDate)
let minute = calendar.component(.minute, from: currentDate)
let second = calendar.component(.second, from: currentDate)
```

これらのコードを実行すると、現在の日付や時刻が取得できます。

```
現在の年: 2021
現在の月: 9
現在の日: 14
現在の曜日: 2 (月曜日)
現在の時: 16
現在の分: 30
現在の秒: 25
```

## 深堀り

Dateクラスにはさまざまな便利なメソッドが用意されています。例えば、指定した日付が日曜日かどうかを判定する`isDateInWeekend()`や、指定した日付と現在の日付の差を取得する`timeIntervalSince(_:)`などがあります。

また、Dateクラスはタイムゾーンやロケールに関する情報も含んでいます。そのため、特定のタイムゾーンやロケールで現在の日付を取得したい場合にも対応することができます。

詳しくは、公式ドキュメントを参考にしてみてください。

## その他参考になる情報

* [Dateクラスの公式ドキュメント](https://developer.apple.com/documentation/foundation/date)