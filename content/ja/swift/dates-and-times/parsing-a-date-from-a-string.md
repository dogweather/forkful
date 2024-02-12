---
title:                "文字列から日付をパースする"
aliases: - /ja/swift/parsing-a-date-from-a-string.md
date:                  2024-02-03T19:15:39.627403-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列から日付をパースする"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/parsing-a-date-from-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となぜ？
文字列から日付を解析するとは、テキストの日付と時間の表現を`Date`オブジェクトに変換するプロセスのことです。このプロセスは、APIレスポンスやユーザ入力など、日付が文字列として伝達されるアプリケーションで不可欠であり、日付の操作やフォーマットを容易にします。

## 方法:

### Foundationの`DateFormatter`を使用する
Swiftの標準ライブラリであるFoundationは、文字列を`Date`オブジェクトに変換する(そしてその逆も)ための`DateFormatter`を提供しています。文字列から日付を解析するには、文字列に一致する日付フォーマットを指定し、そのフォーマッタを使用して解析します。

```swift
import Foundation

let dateString = "2023-04-30"
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
if let date = formatter.date(from: dateString) {
    print("解析された日付: \(date)")
} else {
    print("日付の解析に失敗しました")
}
// サンプル出力: 解析された日付: 2023-04-29 22:00:00 +0000
```

出力はあなたのタイムゾーンに基づいて異なる場合があります。

### ISO8601DateFormatterを使用する
ISO 8601の日付形式のために、Swiftは特化したフォーマッタ、`ISO8601DateFormatter`を提供しており、解析プロセスを簡素化します。

```swift
import Foundation

let dateString = "2023-04-30T15:00:00+00:00"
let isoFormatter = ISO8601DateFormatter()
if let date = isoFormatter.date(from: dateString) {
    print("解析されたISO8601日付: \(date)")
} else {
    print("ISO8601日付の解析に失敗しました")
}
// サンプル出力: 解析されたISO8601日付: 2023-04-30 15:00:00 +0000
```

### サードパーティライブラリを使用する: SwiftDate
Swiftは日付の解析のために堅牢なツールを提供していますが、SwiftDateのようなサードパーティのライブラリはさらに柔軟性と便利さを提供します。プロジェクトにSwiftDateを追加した後、解析は次のようにシンプルになります：

```swift
import SwiftDate

let dateString = "April 30, 2023"
if let date = dateString.toDate("MMMM dd, yyyy") {
    print("SwiftDateを使用して解析された日付: \(date)")
} else {
    print("SwiftDateを使用した日付の解析に失敗しました")
}
// サンプル出力: SwiftDateを使用して解析された日付: 2023-04-30 00:00:00 +0000
```

SwiftDateは自然言語や幅広い日付フォーマットでの解析を簡単にし、Swiftプログラミングツールキットに強力な追加をもたらします。
