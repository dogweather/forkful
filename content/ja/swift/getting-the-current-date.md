---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:16:56.748856-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"

category:             "Swift"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (何とその理由？)
現在の日付を取得するのは、文字通りその時点の日付と時刻を知る手段です。この情報はログ記録、日付のタイムスタンプ付け、またはアプリケーション内のイベントスケジューリングのために頻繁に使われます。

## How to: (方法)
```swift
import Foundation

// 現在の日付と時刻を取得
let now = Date()

// 標準出力への表示
print(now)
```

サンプル出力:
```
2023-04-12 07:46:25 +0000
```

## Deep Dive (詳細解説)
Swiftでは、`Date()` イニシャライザを使って現在の日付と時刻を取得します。これはiOS 2.0以来存在しており、AppleのFoundationフレームワークの一部です。過去にはObjective-Cを使って同じように`[NSDate date]`が利用されましたが、Swiftの登場とともにより簡潔な構文が使用可能になりました。

代替手段としては、Calendar APIを使ってより詳細な情報を抽出したり、特定のタイムゾーンやロケールに対応した日時を取得したりすることができます。

```swift
// カレンダーを使用して、特定のコンポーネントを取り出す。
let calendar = Calendar.current
let year = calendar.component(.year, from: now)
let month = calendar.component(.month, from: now)
let day = calendar.component(.day, from: now)
print("\(year)-\(month)-\(day)")
```

実装時には、システム時刻の代わりにモックを使用することで、テストしやすくなります。これにより、特定の日時をシミュレートすることが可能になり、テストの精度が高まります。

## See Also (関連情報)
- AppleのDateクラスのドキュメント: [https://developer.apple.com/documentation/foundation/date](https://developer.apple.com/documentation/foundation/date)
- SwiftのCalendar APIに関する情報: [https://developer.apple.com/documentation/foundation/calendar](https://developer.apple.com/documentation/foundation/calendar)
- 日付と時刻のフォーマットについて詳しくは、DateFormatterを参照: [https://developer.apple.com/documentation/foundation/dateformatter](https://developer.apple.com/documentation/foundation/dateformatter)
