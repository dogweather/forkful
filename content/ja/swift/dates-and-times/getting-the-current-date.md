---
aliases:
- /ja/swift/getting-the-current-date/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:11:11.117290-07:00
description: "Swift\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u306B\u306F\u3001\u30A2\u30D7\u30EA\u304C\u5B9F\u884C\u3055\u308C\u3066\u3044\u308B\
  \u65E5\u4ED8\u3068\u6642\u523B\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\
  \u306B`Date`\u30AF\u30E9\u30B9\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u30BF\u30A4\u30E0\
  \u30B9\u30BF\u30F3\u30D7\u4ED8\u3051\u3001\u65E5\u4ED8\u8A08\u7B97\u306E\u5B9F\u884C\
  \u3001\u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u306B\
  \u65E5\u4ED8\u3068\u6642\u523B\u3092\u8868\u793A\u3059\u308B\u307E\u3067\u3001\u69D8\
  \u3005\u306A\u7406\u7531\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\
  \u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002"
lastmod: 2024-02-18 23:08:55.239275
model: gpt-4-0125-preview
summary: "Swift\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u306B\
  \u306F\u3001\u30A2\u30D7\u30EA\u304C\u5B9F\u884C\u3055\u308C\u3066\u3044\u308B\u65E5\
  \u4ED8\u3068\u6642\u523B\u306B\u30A2\u30AF\u30BB\u30B9\u3059\u308B\u305F\u3081\u306B\
  `Date`\u30AF\u30E9\u30B9\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30A4\u30D9\u30F3\u30C8\u306E\u30BF\u30A4\u30E0\u30B9\
  \u30BF\u30F3\u30D7\u4ED8\u3051\u3001\u65E5\u4ED8\u8A08\u7B97\u306E\u5B9F\u884C\u3001\
  \u30E6\u30FC\u30B6\u30FC\u30A4\u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u306B\u65E5\
  \u4ED8\u3068\u6642\u523B\u3092\u8868\u793A\u3059\u308B\u307E\u3067\u3001\u69D8\u3005\
  \u306A\u7406\u7531\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\
  \u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002"
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
---

{{< edit_this_page >}}

## 何となく何故？
Swiftで現在の日付を取得するには、アプリが実行されている日付と時刻にアクセスするために`Date`クラスを使用します。プログラマーは、イベントのタイムスタンプ付け、日付計算の実行、ユーザーインターフェイスに日付と時刻を表示するまで、様々な理由で現在の日付を取得する必要があります。

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
