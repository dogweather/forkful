---
title:    "Gleam: 日付を文字列に変換する"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することのメリットは何でしょうか？日付を文字列に変換することで、プログラム内でより柔軟に日付を扱うことができるようになります。例えば、ユーザーから入力された日付をデータベースに保存する際に、文字列に変換するとデータの整合性を保つことができます。また、特定のフォーマットに日付を表示したい場合にも役立ちます。

## 方法

日付を文字列に変換するには、Gleamの標準ライブラリであるdatetimeモジュールを使用します。まず、日付を表す```Date```型の変数を用意します。次に、```Date.to_string```関数を使い、指定したフォーマットに従って日付を文字列に変換します。例としては、以下のようになります。

```Gleam
import gleam/datetime

let date = datetime.parse(2020, 12, 31)
let formatted_date = datetime.to_string(date, "%Y/%m/%d")
```

このコードでは、2020年12月31日を表す```date```変数を、"年/月/日"のフォーマットに従って文字列に変換しています。

## 深堀り

Gleamのdatetimeモジュールでは、日付を文字列に変換する際にさまざまなフォーマットオプションを利用することができます。例えば、曜日や午前/午後の表記、0埋めなどの設定をカスタマイズすることができます。また、日本語表記に対応しているため、日本語の曜日や月名を表示することもできます。細かい設定方法は公式ドキュメントを参照してください。

## 参考

[datetime module - Gleam公式ドキュメント](https://gleam.run/modules/gleam_datetime.html)  
[Date and Time Formatting - Unicode CLDR](https://unicode-org.github.io/cldr-staging/charts/latest/supplemental/dates_times.html)