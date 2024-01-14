---
title:    "Swift: 「日付を文字列に変換する」"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換する理由は様々です。例えば、データベースやAPIとのやり取りで日付を文字列として受け取る必要がある場合や、ユーザーに見やすい形式で日付を表示する必要がある場合などが挙げられます。

## 方法
日付を文字列に変換する方法はSwiftで簡単に実装できます。まず、Date型の変数を用意します。次に、DateFormatterクラスを使用して指定した形式で日付を文字列に変換します。以下のコードを参考にしてください。

```Swift
let date = Date()

let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy/MM/dd" // 必要に応じてフォーマットを変更可能

let dateString = dateFormatter.string(from: date)
print(dateString) // 例：2021/07/12 (今日の日付を表示)
```

このコードを実行すると、指定した形式の日付が文字列として出力されます。

## ディープダイブ
日付を文字列に変換する際、より詳細な設定やオプションを指定することも可能です。たとえば、日付のタイムゾーンやロケールを考慮するなど、国や地域によって異なる表示方法をすることができます。また、カスタムしたフォーマットを使用することもできます。詳細については公式ドキュメントを確認してください。

## 他にも見る価値があるもの
- [Apple公式ドキュメント](https://developer.apple.com/documentation/foundation/dateformatter)
- [Qiitaの日付を文字列に変換する方法についての記事](https://qiita.com/cf_pie/items/1b44b7a5994a9d8b0e84)