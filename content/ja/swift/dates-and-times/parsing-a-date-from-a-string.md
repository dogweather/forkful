---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:39.627403-07:00
description: "\u65B9\u6CD5: Swift\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3067\
  \u3042\u308BFoundation\u306F\u3001\u6587\u5B57\u5217\u3092`Date`\u30AA\u30D6\u30B8\
  \u30A7\u30AF\u30C8\u306B\u5909\u63DB\u3059\u308B(\u305D\u3057\u3066\u305D\u306E\u9006\
  \u3082)\u305F\u3081\u306E`DateFormatter`\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\
  \u3059\u3002\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u306B\u306F\u3001\u6587\u5B57\u5217\u306B\u4E00\u81F4\u3059\u308B\u65E5\u4ED8\u30D5\
  \u30A9\u30FC\u30DE\u30C3\u30C8\u3092\u6307\u5B9A\u3057\u3001\u305D\u306E\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30BF\u3092\u4F7F\u7528\u3057\u3066\u89E3\u6790\u3057\u307E\u3059\
  \u3002"
lastmod: '2024-04-05T21:53:43.426060-06:00'
model: gpt-4-0125-preview
summary: "Swift\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u3067\u3042\u308B\
  Foundation\u306F\u3001\u6587\u5B57\u5217\u3092`Date`\u30AA\u30D6\u30B8\u30A7\u30AF\
  \u30C8\u306B\u5909\u63DB\u3059\u308B(\u305D\u3057\u3066\u305D\u306E\u9006\u3082\
  )\u305F\u3081\u306E`DateFormatter`\u3092\u63D0\u4F9B\u3057\u3066\u3044\u307E\u3059\
  \u3002\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\u306B\
  \u306F\u3001\u6587\u5B57\u5217\u306B\u4E00\u81F4\u3059\u308B\u65E5\u4ED8\u30D5\u30A9\
  \u30FC\u30DE\u30C3\u30C8\u3092\u6307\u5B9A\u3057\u3001\u305D\u306E\u30D5\u30A9\u30FC\
  \u30DE\u30C3\u30BF\u3092\u4F7F\u7528\u3057\u3066\u89E3\u6790\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

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
