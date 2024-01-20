---
title:                "2つの日付を比較する"
html_title:           "Elixir: 2つの日付を比較する"
simple_title:         "2つの日付を比較する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付の比較とは、2つの日付が同じ日か、またはその順番を確認するプログラムの手順です。プログラマーは、タスクの期限、イベントのスケジュール、または時間の流れをトラックするために日付の比較を行います。

## 手順：

Swift言語での日付の比較は、`compare()`メソッドまたは`==`、`<`、`>`などの演算子を使用します。

例えば：

```Swift
import Foundation

let formatter = DateFormatter()
formatter.dateFormat = "yyyy/MM/dd HH:mm"
let date1 = formatter.date(from: "2022/03/22 14:20")!
let date2 = formatter.date(from: "2022/03/27 10:15")!

if date1.compare(date2) == .orderedSame {
    print("Two dates are same")
} else if date1.compare(date2) == .orderedAscending {
    print("Date1 is earlier than Date2")
} else {
    print("Date1 is later than Date2")
}
```
このプログラムは`Date1 is earlier than Date2`と出力します。

## 詳細な内容

日付の比較機能は、コンピューターが時間と日付をほとんど全ての部分で使っているため、必然的に開発されました。他のプログラミング言語では、日付の比較にはいくつかの異なる手法が存在しますが、Swiftでは`Date`オブジェクトと組み込みの比較オペレータをシンプルに使用します。

日付の正確な比較のため、Swiftは時間帯やカレンダーシステムの差異を考慮して計算をおこないます。そのため、たとえば西暦と和暦を比較しても期待通りの結果が返ります。

## 参考情報

次のリンクは日付の比較、操作、および他の関連するトピックについての詳細を提供します：

1. Appleの公式ドキュメンテーション: [Date - Foundation](https://developer.apple.com/documentation/foundation/date)