---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:36:58.004701-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
何となぜ？

文字列から日付を解析するとは、日付のデータを文字列から抽出して利用可能な形式に変換することです。プログラマーはデータ収集、ログ分析、ユーザー入力の処理などで使用します。

## How to:
やり方

```Gleam
import gleam/calendar
import gleam/should

pub fn parse_date(date_string: String) -> Result(calendar.Date, String) {
  calendar.Date.from_iso8601_string(date_string)
}

fn main() {
  let date_example = "2023-04-02"
  case parse_date(date_example) {
    Ok(date) -> should.equal(date, calendar.Date(2023, 04, 02))
    Error(err) -> io.println(err)
  }
}
```

出力サンプル：
```
Ok(#Date(year: 2023, month: 4, day: 2))
```

## Deep Dive
深掘り

文字列から日付を解析する機能は、いずれのプログラミング言語にも共通です。歴史的には、多様な日付フォーマットが使われてきましたが、ISO 8601は国際的な標準フォーマットとして広く受け入れられています。Gleamでは、`calendar`モジュールを使いISO 8601形式の日付文字列から日付型へ確実に変換できます。代替手段として、独自の解析規則を設けることも可能ですが、標準フォーマットの使用が推奨されます。Gleamの型安全な性質は、期待するパース結果をしっかり保証してくれるので、解析エラーを適切に扱うことが重要です。

## See Also
関連情報

- ISO 8601 Date and time format overview: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
