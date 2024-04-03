---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:11.117290-07:00
description: "\u65B9\u6CD5\uFF1A Swift\u306E`Foundation`\u30D5\u30EC\u30FC\u30E0\u30EF\
  \u30FC\u30AF\u306F\u3001`Date`\u30AF\u30E9\u30B9\u3092\u63D0\u4F9B\u3057\u3066\u304A\
  \u308A\u3001\u73FE\u5728\u306E\u65E5\u4ED8\u3068\u6642\u523B\u3092\u7C21\u5358\u306B\
  \u53D6\u5F97\u3067\u304D\u307E\u3059\u3002\u4EE5\u4E0B\u304C\u73FE\u5728\u306E\u65E5\
  \u4ED8\u3092\u53D6\u5F97\u3059\u308B\u305F\u3081\u306E\u57FA\u672C\u7684\u306A\u4F8B\
  \u3067\u3059\uFF1A."
lastmod: '2024-03-13T22:44:42.631416-06:00'
model: gpt-4-0125-preview
summary: "Swift\u306E`Foundation`\u30D5\u30EC\u30FC\u30E0\u30EF\u30FC\u30AF\u306F\u3001\
  `Date`\u30AF\u30E9\u30B9\u3092\u63D0\u4F9B\u3057\u3066\u304A\u308A\u3001\u73FE\u5728\
  \u306E\u65E5\u4ED8\u3068\u6642\u523B\u3092\u7C21\u5358\u306B\u53D6\u5F97\u3067\u304D\
  \u307E\u3059\u3002\u4EE5\u4E0B\u304C\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\
  \u3059\u308B\u305F\u3081\u306E\u57FA\u672C\u7684\u306A\u4F8B\u3067\u3059\uFF1A."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 方法：
Swiftの`Foundation`フレームワークは、`Date`クラスを提供しており、現在の日付と時刻を簡単に取得できます。以下が現在の日付を取得するための基本的な例です：

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

これは次のような出力になります：

```
2023-04-12 07:46:23 +0000
```

出力形式はISO 8601標準に従い、UTCタイムゾーンを使用します。しかし、表示目的でこの日付をフォーマットしたい場合があります。Swiftの`DateFormatter`クラスが救世主となります：

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

例の出力はこんな感じです：

```
2023年4月12日 午前10:46:23
```

出力形式は、コードを実行しているデバイスのロケールによって異なることに注意してください。

より複雑な日付操作が必要なプロジェクトでは、多くのSwift開発者が`SwiftDate`といったサードパーティのライブラリーを活用しています。特定のタイムゾーンと形式で現在の日付を取得するために`SwiftDate`を使用する方法は次のとおりです：

最初に、SPM、CocoaPods、またはCarthageを使用してプロジェクトに`SwiftDate`を追加します。その後：

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

これは次のような出力になります：

```
2023-04-12 09:46:23
```

`SwiftDate`を使用すると、異なるタイムゾーンやロケールの日付と時刻を簡単に操作でき、Swiftアプリケーションで複雑な日付処理タスクを簡素化できます。
