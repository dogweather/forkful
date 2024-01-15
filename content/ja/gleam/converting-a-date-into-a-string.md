---
title:                "「日付を文字列に変換する」"
html_title:           "Gleam: 「日付を文字列に変換する」"
simple_title:         "「日付を文字列に変換する」"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換することの利点は、プログラムで日付データを扱うために必要です。例えば、データベースに日付を格納する場合や、ユーザーに表示する場合などには、日付を理解しやすい形式に変換する必要があります。Gleamでは、簡単な方法で日付を文字列に変換することができます。

## ハウツー
日付型から文字列型に変換する方法は、以下のようになります。

```Gleam
import Time.Date
import Time.Format

let date = Time.Date(2021, 8, 17)

let formatted_date = Time.Format.to_string(date, "%Y-%m-%d")

IO.print("変換後の日付: " ++ formatted_date)
```

このコードを実行すると、以下のような出力が得られます。

```
変換後の日付: 2021-08-17
```

Gleamでは、 `Time.Format` モジュールを使用して日付を文字列にフォーマットすることができます。 `to_string` 関数には、フォーマットしたい日付とフォーマット文字列を渡すことで、日付を任意の形式に変換することができます。

## ディープダイブ
日付を文字列に変換する際には、フォーマット文字列をどのように指定するかが重要です。Gleamでは、 `strftime` という関数を使用して、日付と時刻を指定したフォーマットに整形することができます。また、日本語を含むフォーマット文字列もサポートされています。

日付を文字列に変換する方法は他にもありますが、Gleamが提供する `Time.Format` モジュールを使用すれば、より簡単に日付を任意の形式に変換することができます。

## 関連情報
[Gleam公式ドキュメント](https://gleam.run/core/time.html#format)