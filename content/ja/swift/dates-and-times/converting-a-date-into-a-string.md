---
date: 2024-01-20 17:37:28.656185-07:00
description: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u3044\u3046\u306E\u306F\u3001`Date` \u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u30C6\
  \u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u5909\u3048\u308B\u30D7\u30ED\u30BB\u30B9\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30E6\u30FC\u30B6\u30FC\u306B\
  \u89AA\u3057\u307F\u3084\u3059\u3044\u5F62\u3067\u65E5\u4ED8\u30C7\u30FC\u30BF\u3092\
  \u8868\u793A\u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u65E5\u4ED8\u3092\u6587\
  \u5B57\u5217\u5F62\u5F0F\u3067\u4FDD\u5B58\u30FB\u901A\u4FE1\u3059\u308B\u305F\u3081\
  \u306B\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:42.632963-06:00'
model: gpt-4-1106-preview
summary: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B\u3068\
  \u3044\u3046\u306E\u306F\u3001`Date` \u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u3092\u30C6\
  \u30AD\u30B9\u30C8\u5F62\u5F0F\u306B\u5909\u3048\u308B\u30D7\u30ED\u30BB\u30B9\u3067\
  \u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\u30E6\u30FC\u30B6\u30FC\u306B\
  \u89AA\u3057\u307F\u3084\u3059\u3044\u5F62\u3067\u65E5\u4ED8\u30C7\u30FC\u30BF\u3092\
  \u8868\u793A\u3059\u308B\u305F\u3081\u3001\u307E\u305F\u306F\u65E5\u4ED8\u3092\u6587\
  \u5B57\u5217\u5F62\u5F0F\u3067\u4FDD\u5B58\u30FB\u901A\u4FE1\u3059\u308B\u305F\u3081\
  \u306B\u884C\u3044\u307E\u3059\u3002."
title: "\u65E5\u4ED8\u3092\u6587\u5B57\u5217\u306B\u5909\u63DB\u3059\u308B"
weight: 28
---

## What & Why? (何となぜ？)

日付を文字列に変換するというのは、`Date` オブジェクトをテキスト形式に変えるプロセスです。プログラマーはユーザーに親しみやすい形で日付データを表示するため、または日付を文字列形式で保存・通信するために行います。

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
