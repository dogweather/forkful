---
title:                "Swift: 現在の日付を取得する"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

日常生活で、時計やカレンダーを見ることなく現在の日付を取得する必要があることがあります。Swiftで簡単に現在の日付を取得する方法を学びましょう。

## 使い方

```Swift
let today = Date() // 現在の日付を取得
let formatter = DateFormatter()
formatter.dateStyle = .medium // 日付を中規模の形式で表示
let formattedDate = formatter.string(from: today) // 文字列に変換
print(formattedDate) // 結果: 2021/03/24
```

## ディープダイブ

日付を取得する方法も、Swiftでは様々な方法があります。例えば、現在の日付から1週間後や1ヶ月後の日付を取得することも可能です。また、タイムゾーンやロケールを指定することで、その地域の日付を取得することもできます。詳細は公式ドキュメントを参照してください。

## 関連リンク

[Swift公式ドキュメント](https://developer.apple.com/documentation/foundation/date)  
[NSDateFormatterクラス](https://developer.apple.com/documentation/foundation/nsdateformatter)