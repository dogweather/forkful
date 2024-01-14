---
title:                "Gleam: 日付の比較"
simple_title:         "日付の比較"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# なぜ日付を比較するのか

日付を比較することは、プログラミングでよく使用される重要なタスクです。例えば、データベース内の日付を比較することで、特定の期間のデータを抽出することができます。日付を比較することで、データの分析やレポート作成などの作業がより効率的に行えるようになります。

## 方法

日付を比較する方法は、いくつかありますが、Gleamプログラミング言語を使用することで簡単に実現することができます。以下のコードを参考にして、日付を比較する方法を学んでみましょう。

```
Gleam.import math/calendar

fn compare_dates(date1, date2) {
    date1 < date2
}

Gleam.test "date comparison" {
    assert compare_dates(calendar.parse_date("2021-01-01"), calendar.parse_date("2021-01-05")) == true
    assert compare_dates(calendar.parse_date("2021-01-01"), calendar.parse_date("2020-12-31")) == false
}
```

上記の例では、`calendar`モジュールを使用して日付をパースし、`compare_dates`関数で比較しています。`Gleam.test`を使用することで、結果を確認することができます。ここでは、日付の比較結果が正しいかどうかをテストしています。

## 深堀り

日付を比較する際には、以下の点に注意する必要があります。

- 日付フォーマットの違い：異なる形式の日付を比較する場合は、事前に適切なフォーマットに変換する必要があります。
- 時間の扱い：日付と共に時間も比較する場合は、厳密に比較する必要があります。Gleamでは、`Time`モジュールを使用することで時間の比較を行うことができます。
- 比較結果の扱い：比較結果は真偽値(`true`or `false`)で返されるため、条件分岐に使用することができます。

以上の点を押さえておくことで、日付を比較する際に生じるエラーを防ぐことができます。

# See Also

- [Gleamの公式ドキュメント](https://gleam.run/)
- [Gleamで日付を扱う](https://gleam.run/libraries/math/calendar)

日付を比較することで、プログラミングの幅がぐっと広がります。ぜひ上記のリンクを参考にしながら、日付を比較する力を身につけてください。