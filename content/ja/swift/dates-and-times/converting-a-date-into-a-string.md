---
date: 2024-01-20 17:37:28.656185-07:00
description: "How to: (\u65B9\u6CD5) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.632963-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## How to: (方法)


### 日付を文字列に変換する
```swift
import Foundation

let now = Date()
let formatter = DateFormatter()

formatter.dateStyle = .medium
formatter.timeStyle = .short

let dateString = formatter.string(from: now)

print(dateString)
// 出力例: "2023年4月20日 18:30"
```

日付のフォーマットはカスタマイズ可能：

```swift
formatter.dateFormat = "yyyy/MM/dd HH:mm"
let customDateString = formatter.string(from: now)

print(customDateString)
// 出力例: "2023/04/20 18:30"
```

## Deep Dive (掘り下げ)


### 歴史的背景
Swiftの前身であるObjective-C時代から、開発者たちは`NSDateFormatter`を用いて日付と文字列の変換を行ってきました。Swiftではこれが`DateFormatter`クラスに進化しました。

### 代替案
Swiftには`DateFormatter`の他にISO8601DateFormatterや相対日付表示のための`RelativeDateTimeFormatter`もあります。状況に応じた使い分けが重要です。

### 実装の詳細
フォーマッタの`locale`プロパティを変更することで、特定のロケール（言語・地域）に応じた日付表示が可能。また、`timeZone`プロパティを使って、異なるタイムゾーンに対応することもできます。

## See Also (参照)
- [DateFormatter - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [Working with Dates and Times in Swift – Ray Wenderlich](https://www.raywenderlich.com/5539282-working-with-dates-and-times-in-swift)
