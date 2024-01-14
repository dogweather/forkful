---
title:                "Swift: 「日付を文字列に変換する」"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換するメリットは、日付をより視覚的に分かりやすい形式で表示することができることです。また、データベースやサーバーに保存する際にも、文字列形式の方が扱いやすい場合があります。

## 方法

```Swift
let date = Date() // 現在の日付を取得
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd" // 任意のフォーマットを設定
let dateString = dateFormatter.string(from: date) // 日付を文字列に変換
print(dateString) // 出力結果：2021/09/16
```

## 深堀り

日付を文字列に変換する際には、フォーマットを適切に設定することが重要です。Swiftでは`DateFormatter`というクラスを使用して、日付のフォーマットを指定することができます。上記の例では`"yyyy/MM/dd"`というフォーマットを設定しましたが、これを変更することでさまざまな形式の文字列を作ることができます。詳細なフォーマットの指定方法については、公式ドキュメントを参照してください。

## 参考

- [Apple Developer Documentation - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [How to convert a date to a different format in Swift?](https://www.hackingwithswift.com/example-code/system/how-to-convert-a-date-to-a-different-format-using-dateformatter)