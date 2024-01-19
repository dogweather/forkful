---
title:                "現在の日付を取得する"
html_title:           "PowerShell: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何と何のために？
現在の日付を取得するとは、現在の日時をコンピュータのシステムクロックから取得することです。プログラマーがこれを行うのは、何がいつ起こったかを記録する、時間に基づいた処理を行う、ユーザーへの日付と時間の表示など、様々なタスクを処理するためです。

## どうするの？
Swiftで現在の日付と時間を取得するには以下のコード例を参考にしてください。

```Swift
import Foundation

let now = Date()

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let stringDate = dateFormatter.string(from: now)

print(stringDate)
```
このコードは現在の日付と時間を`yyyy-MM-dd HH:mm:ss`というフォーマットで表示します。

## 詳細解説
- 歴史的な背景：計算機が登場する以前、時間は人力で追跡されていました。しかし、計算機の登場により、時間は機械的にきちんと追跡することができるようになりました。
- その他の手段：現在の日付と時間を取得する他の方法としては、NSCalendarやNSDateComponentsなどを使用する方法もありますが、Dateはそのような複雑さを排除し、簡潔に現在の日付と時間を取得するための方法を提供しています。
- 実装の詳細：Dateは1970年以降の時間を秒単位で表示します。これにより、日付と時間の情報を数値として操作することが可能になっています。

## 参照リンク
以下のリンクは関連する情報源へのリンクです。

- [Apple Developer Documentation](https://developer.apple.com/documentation)
- [Swift Date official documentation](https://developer.apple.com/documentation/foundation/date)
- [DateFormatter official documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [NSDateComponents official documentation](https://developer.apple.com/documentation/foundation/nsdatecomponents)