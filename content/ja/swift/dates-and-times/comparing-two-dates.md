---
date: 2024-01-20 17:33:52.972649-07:00
description: "\u6BD4\u8F03\u3059\u308B\u4E8C\u3064\u306E\u65E5\u4ED8\u3068\u306F\u3001\
  \u57FA\u672C\u7684\u306B\u3069\u3061\u3089\u304C\u524D\u3067\u3069\u3061\u3089\u304C\
  \u5F8C\u304B\u3092\u5224\u65AD\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A4\u30D9\u30F3\u30C8\u304C\u767A\u751F\u3059\
  \u308B\u9806\u5E8F\u3092\u7BA1\u7406\u3057\u305F\u308A\u3001\u671F\u9650\u304C\u5B88\
  \u3089\u308C\u3066\u3044\u308B\u304B\u3092\u78BA\u8A8D\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:40.582165-07:00'
model: gpt-4-1106-preview
summary: "\u6BD4\u8F03\u3059\u308B\u4E8C\u3064\u306E\u65E5\u4ED8\u3068\u306F\u3001\
  \u57FA\u672C\u7684\u306B\u3069\u3061\u3089\u304C\u524D\u3067\u3069\u3061\u3089\u304C\
  \u5F8C\u304B\u3092\u5224\u65AD\u3059\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30A4\u30D9\u30F3\u30C8\u304C\u767A\u751F\u3059\
  \u308B\u9806\u5E8F\u3092\u7BA1\u7406\u3057\u305F\u308A\u3001\u671F\u9650\u304C\u5B88\
  \u3089\u308C\u3066\u3044\u308B\u304B\u3092\u78BA\u8A8D\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002"
title: "\u65E5\u4ED8\u3092\u6BD4\u8F03\u3059\u308B"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
比較する二つの日付とは、基本的にどちらが前でどちらが後かを判断することです。プログラマーは、イベントが発生する順序を管理したり、期限が守られているかを確認するためにこれを行います。

## How to (やり方):
Swiftでは、`Date` オブジェクトを簡単に比較できます。例を見てみましょう。

```Swift 
import Foundation

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd HH:mm"
guard let date1 = dateFormatter.date(from: "2023/03/01 09:00"),
      let date2 = dateFormatter.date(from: "2023/03/02 20:00") else {
    fatalError("Incorrect date format")
}

// 日付を比較する
if date1 < date2 {
  print("date1 is earlier than date2")
} else if date1 > date2 {
  print("date1 is later than date2")
} else {
  print("date1 and date2 are at the same moment")
}

// 出力: date1 is earlier than date2
```

## Deep Dive (深掘り):
日付の比較は、UNIX時代からの基礎的な操作です。`NSDate` がSwiftの前身であるObjective-Cで用いられていましたが、Swiftでは `Date` 型が導入され、使いやすくなりました。本質的に、日付は内部的にタイムスタンプ（エポックからの秒数）として格納されます。これによって、必要な算術演算や比較が可能になります。

他の言語では、様々な日付と時間のライブラリがありますが、Swiftは `Date` オブジェクトと一緒に `Calendar` を使うことで、より高度な操作が可能です。例えば、特定のタイムゾーンでの比較や、カレンダー単位での差を取るなどです。

実装の詳細では、比較演算子 `<, >, ==` は `Date` の `Comparable` と `Equatable` プロトコルによって自然に働きます。これにより、日付を配列内でソートしたり、条件文で直接扱ったりできるようになります。

## See Also (関連情報):
- AppleのDateドキュメント: [https://developer.apple.com/documentation/foundation/date](https://developer.apple.com/documentation/foundation/date)
- タイムゾーンやカレンダーによる比較についてのNSHipsterの記事: [https://nshipster.com/datecomponents/](https://nshipster.com/datecomponents/) 
- Swiftの公式ガイド（英語）: [https://docs.swift.org/swift-book/LanguageGuide/CollectionTypes.html#//apple_ref/doc/uid/TP40014097-CH8-ID105](https://docs.swift.org/swift-book/LanguageGuide/CollectionTypes.html#//apple_ref/doc/uid/TP40014097-CH8-ID105)
