---
title:                "現在の日付の取得"
aliases:
- ja/swift/getting-the-current-date.md
date:                  2024-02-03T19:11:11.117290-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
