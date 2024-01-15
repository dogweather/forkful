---
title:                "日付を文字列に変換する"
html_title:           "Rust: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ

日付を文字列に変換することのメリットは何でしょうか？Rustの最新バージョンである今、その理由を簡潔に説明します。

## ハウツー

日付を文字列に変換するには、 `format!` マクロを使用します。これは、指定したフォーマットで日付を文字列に変換することができます。例えば、今日の日付を `YYYY/MM/DD` の形式で表示するコードは以下のようになります。

```Rust 
use chrono::{DateTime, Local, Datelike, Timelike};

// 今日の日付を取得
let today = Local::now();

// 日付を `YYYY/MM/DD` の形式で文字列に変換
let formatted_date = format!("{}/{}/{}", today.year(), today.month(), today.day());

println!("{}", formatted_date); // Output: 2021/08/26
```

さらに、曜日や時刻などの情報を含めて変換することもできます。詳細については、公式ドキュメントを参照してください。

## ディープダイブ

日付を文字列に変換する方法は、タイムゾーンやロケールなどの要因によって異なります。Rustでは、このような多様な要因に対応するために、`chrono`ライブラリを使用することができます。このライブラリには、日付や時刻を操作するための様々な機能が備わっています。

## See Also

- [The Rust Programming Language](https://www.rust-lang.org/)
- [Official Rust Documentation](https://doc.rust-lang.org/)
- [Chrono Documentation](https://docs.rs/chrono/latest/)