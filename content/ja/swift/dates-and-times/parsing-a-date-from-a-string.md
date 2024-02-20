---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:39.627403-07:00
description: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u65E5\u4ED8\u3068\u6642\u9593\u306E\
  \u8868\u73FE\u3092`Date`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u5909\u63DB\u3059\
  \u308B\u30D7\u30ED\u30BB\u30B9\u306E\u3053\u3068\u3067\u3059\u3002\u3053\u306E\u30D7\
  \u30ED\u30BB\u30B9\u306F\u3001API\u30EC\u30B9\u30DD\u30F3\u30B9\u3084\u30E6\u30FC\
  \u30B6\u5165\u529B\u306A\u3069\u3001\u65E5\u4ED8\u304C\u6587\u5B57\u5217\u3068\u3057\
  \u3066\u4F1D\u9054\u3055\u308C\u308B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u3067\u4E0D\u53EF\u6B20\u3067\u3042\u308A\u3001\u65E5\u4ED8\u306E\u64CD\u4F5C\u3084\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:01.740061
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u3068\u306F\u3001\u30C6\u30AD\u30B9\u30C8\u306E\u65E5\u4ED8\u3068\u6642\u9593\u306E\
  \u8868\u73FE\u3092`Date`\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u306B\u5909\u63DB\u3059\
  \u308B\u30D7\u30ED\u30BB\u30B9\u306E\u3053\u3068\u3067\u3059\u3002\u3053\u306E\u30D7\
  \u30ED\u30BB\u30B9\u306F\u3001API\u30EC\u30B9\u30DD\u30F3\u30B9\u3084\u30E6\u30FC\
  \u30B6\u5165\u529B\u306A\u3069\u3001\u65E5\u4ED8\u304C\u6587\u5B57\u5217\u3068\u3057\
  \u3066\u4F1D\u9054\u3055\u308C\u308B\u30A2\u30D7\u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\
  \u3067\u4E0D\u53EF\u6B20\u3067\u3042\u308A\u3001\u65E5\u4ED8\u306E\u64CD\u4F5C\u3084\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3092\u5BB9\u6613\u306B\u3057\u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
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
