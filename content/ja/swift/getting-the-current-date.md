---
title:    "Swift: 現在日付を取得する"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## なぜ日付を取得するのか

プログラミングにおいて、現在の日付を取得することは非常に重要です。例えば、あるイベントの作成日時を記録したい場合や、期限を設定する際に必要になるためです。

## 方法

Swiftでは、Date()という組み込みの構造体を使用して、現在の日付を取得することができます。次のコードを使用することで、年、月、日、時、分、秒のそれぞれの値を取得することができます。

```Swift
let date = Date()
let calendar = Calendar.current

let year = calendar.component(.year, from: date)
let month = calendar.component(.month, from: date)
let day = calendar.component(.day, from: date)
let hour = calendar.component(.hour, from: date)
let minute = calendar.component(.minute, from: date)
let second = calendar.component(.second, from: date)

print("年: \(year)")
print("月: \(month)")
print("日: \(day)")
print("時: \(hour)")
print("分: \(minute)")
print("秒: \(second)")
```

上記のコードを実行すると、現在の日付に関する情報が出力されます。

```
年: 2020
月: 7
日: 15
時: 14
分: 30
秒: 25
```

## ディープダイブ

Date()を使用すると、現在の日付だけでなく、過去や未来の日付を取得することも可能です。また、タイムゾーンやロケールを指定することで、より正確な日付を取得することができます。

さらに、Date()が扱う日付はグレゴリオ暦であり、太陽の周期に基づいて算出されています。そのため、うるう年や時差などの特殊な計算も考慮されています。

## 他にも参考になる記事

[公式ドキュメント - Date](https://developer.apple.com/documentation/foundation/date)  
[How to Get Current Date and Time in Swift](https://www.techotopia.com/index.php/Working_with_Dates_and_Times_in_Swift_3)  
[Manipulating and Formatting Dates and Times in Swift](https://developer.apple.com/forums/thread/92389)