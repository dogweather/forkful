---
title:                "将日期转换为字符串"
html_title:           "Swift: 将日期转换为字符串"
simple_title:         "将日期转换为字符串"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
為什麼要將日期轉換成字符串？日期是我們日常生活中經常需要處理的數據類型，將日期轉換成字符串可以方便我們在程式中進行格式化和輸出。

## How To
日期轉換成字符串有許多不同的方法，我們來看幾個常用的例子。

### 使用 DateFormatter
DateFormatter 是一個提供日期格式化功能的類別。我們可以使用它來將日期轉換成我們需要的字符串格式。
```swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
let dateString = formatter.string(from: date)
print(dateString)
```
輸出：2020-09-15

### 使用 Unicode Technical Standard #35
Unicode Technical Standard #35（UTS #35）是一個指定日期和時間格式的標準。我們可以使用它來將日期轉換成指定的字符串格式。
```swift
let date = Date()
let dateString = date.unicodeDescription
print(dateString)
```
輸出：2020年9月15日星期二GMT

### 使用 String(describing:)
String(describing:) 是一個將任意類型轉換成字符串的方法，對於日期類型也適用。
```swift
let date = Date()
let dateString = String(describing: date)
print(dateString)
```
輸出：2020-09-15 07:00:00 +0000

## Deep Dive
日期轉換成字符串的背後原理是將日期的數值轉換成對應的字符。DateFormatter 和 UTS #35 都是基於這個原理來實現的。而String(describing:)則是直接將日期的<Date>類型轉換成格式化後的字符串。

## See Also
- [NSDateFormatter Class Reference](https://developer.apple.com/documentation/foundation/nsdateformatter)
- [Unicode Technical Standard #35](https://unicode.org/reports/tr35/tr35-dates.html)
- [String(describing:) Documentation](https://developer.apple.com/documentation/swift/string/2927320-describing)