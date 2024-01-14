---
title:    "Swift: 「日付を文字列に変換する」"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することを行う理由は、アプリケーションで日付を表示したり、データベースに保存するためです。

## 方法

```Swift
let date = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd"
let dateString = dateFormatter.string(from: date)
// Output: "2021/01/01"
```

上記の例では、Dateオブジェクトを生成し、DateFormatterを使用して指定した形式で日付を文字列に変換しています。"yyyy/MM/dd"の部分は日付のフォーマットを指定するため、必要に応じて変更することができます。さらに、TimezoneやLocaleなどのオプションを指定することもできます。

## ディープダイブ

日付を文字列に変換する際には、時間やタイムゾーン、ロケールなどのオプションを指定することができます。また、DateFormatterを使用するだけでなく、String interpolationやNSCalendar等の別の方法でも日付を文字列に変換することができます。

## 参考リンク

[Apple Developer - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter) \
[Hacking with Swift - How to convert a Date to a String](https://www.hackingwithswift.com/example-code/system/how-to-convert-a-date-to-a-string)