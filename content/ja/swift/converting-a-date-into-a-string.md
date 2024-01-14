---
title:                "Swift: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

プログラマーであるなら、日付を文字列に変換する必要があるかもしれません。日付を特定の形式で表示したい、データベースに保存するために文字列に変換したい、などの理由が考えられます。

## 方法

日付を文字列に変換するには、以下のようなコードを使用します。

```Swift
let date = Date() // 現在の日付を取得
let formatter = DateFormatter()
formatter.dateFormat = "yyyy/MM/dd" // 出力する日付のフォーマットを設定
let dateString = formatter.string(from: date) // 文字列に変換
print(dateString) // 出力: "2020/07/09"
```

上記の例では、DateFormatterクラスを使用して日付を文字列に変換しています。`dateFormat`プロパティを設定することで、出力する日付のフォーマットを指定することができます。より詳細なフォーマットについては、下の"深堀り"セクションをご覧ください。

## 深堀り

日付をフォーマットする際に使用できる代表的な文字列をいくつか紹介します。

- `yyyy`: 4桁の年 (例: "2020")
- `MM`: 2桁の月 (例: "07")
- `dd`: 2桁の日 (例: "09")
- `h`: 12時間制の時 (例: "9")
- `hh`: 12時間制の時 (例: "09")
- `H`: 24時間制の時 (例: "19")
- `HH`: 24時間制の時 (例: "19")
- `m`: 分 (例: "5")
- `mm`: 分 (例: "05")
- `a`: a.m./p.m.を表す (例: "a.m.")
- `E`: 曜日 (例: "木曜日")

詳細なフォーマットについては、[Unicode Technical Standard #35](https://www.unicode.org/reports/tr35/tr35-31/tr35-dates.html#Date_Format_Patterns)を参照してください。

## また見る

- [DateFormatter Class Reference](https://developer.apple.com/documentation/foundation/dateformatter)
- [Unicode Technical Standard #35](https://www.unicode.org/reports/tr35/tr35-31/tr35-dates.html#Date_Format_Patterns)