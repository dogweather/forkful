---
title:                "Swift: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得する理由は何でしょうか？日付は、アプリケーション内で各種のタイムスタンプを作成し、アクションを行う際に必要となる重要な情報です。また、ユーザーにとっても重要な情報であり、アプリケーションの使いやすさを向上させることができます。

## 方法

Swiftを使用して、現在の日付を取得する方法を紹介します。まず、Dateオブジェクトを使用して現在の日付を取得します。

```Swift
let date = Date()
```

次に、日付を特定の書式で表示するために、DateFormatterオブジェクトを使用します。

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
let formattedDate = dateFormatter.string(from: date)

print(formattedDate) // 2021-01-01
```

また、現在の日付に加算や減算を行うこともできます。例えば、10日後の日付を取得する場合は、DateComponentsオブジェクトを使用して以下のように計算することができます。

```Swift
var dateComponents = DateComponents()
dateComponents.day = 10
let futureDate = Calendar.current.date(byAdding: dateComponents, to: date)

print(futureDate) // 現在の日付から10日後の日付
```

## 深堀り

Dateオブジェクトは実際には単なるタイムスタンプであり、タイムゾーンやカレンダーの情報を持ちません。実際には、DateFormatterやCalendarを使用して、日付を表現するためのフォーマットやローカライズされた日付を取得する必要があります。

また、Dateオブジェクトはミリ秒単位の精度を持っていますが、一部のアプリケーションでは、より高い精度が必要になる場合があります。そのような場合は、TimeIntervelやCFAbsoluteTimeなど、より細かい精度のタイムスタンプを使用することができます。

## 参考

- [Date - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/date)
- [DateFormatter - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [Calendar - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/calendar)
- [TimeIntervel - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/timeinterval)
- [CFAbsoluteTime - Apple Developer Documentation](https://developer.apple.com/documentation/corefoundation/cfabsolutetime)