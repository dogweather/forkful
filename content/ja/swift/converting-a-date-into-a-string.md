---
title:                "「日付を文字列に変換する」"
html_title:           "Swift: 「日付を文字列に変換する」"
simple_title:         "「日付を文字列に変換する」"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することの意義は、私たちが日常的に使う日付や時刻の表現を柔軟に扱うことができるようにすることにあります。例えば、アプリケーションで過去の予定を確認したり、データベースに保存されている日付をユーザーが理解しやすい形式で表示したりする際に便利です。

## コーディング例

以下のようなコードを使って、日付を文字列に変換することができます。

```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "yyyy年MM月dd日"
let dateString = formatter.string(from: date)
print(dateString) // 出力: 2021年03月23日
```

また、`yyyy`や`MM`、`dd`といったフォーマットを変更することで、様々な日付の表現を作ることができます。詳細なコーディング例やフォーマットの意味については、[公式ドキュメント](https://developer.apple.com/documentation/foundation/dateformatter)を参考にしてください。

## ディープダイブ

日付を文字列に変換するときには、[カレンダー](https://developer.apple.com/documentation/foundation/calendar)を使用して、ユーザーが設定している地域やタイムゾーンに応じたフォーマットを生成することができます。また、`DateFormatter`のオプションを使って、日付の曜日や時刻を含めることもできます。

## 参考リンク

- [DateFormatter - 公式ドキュメント](https://developer.apple.com/documentation/foundation/dateformatter)
- [Calendar - 公式ドキュメント](https://developer.apple.com/documentation/foundation/calendar)