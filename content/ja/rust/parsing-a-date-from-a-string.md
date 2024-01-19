---
title:                "文字列から日付を解析する"
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## 何となぜ？

日付からの文字列解析（parsing a date from a string）は、文字列形式の日付を特定の日付型に変換する処理です。これは、データ入力、API応答、またはファイルから日付を取り出すときなど、様々な場面でプログラマーによって行われます。

## やり方：

以下にRustのコード例とその出力結果を示します。

```Rust
use chrono::{DateTime, NaiveDate, NaiveDateTime};
use chrono::format::ParseError;

fn parse_date_from_string(date_string: &str) -> Result<NaiveDate, ParseError> {
    let date = NaiveDate::parse_from_str(date_string, "%Y-%m-%d");
    date
}

fn main() {
    let date_str = "2022-03-11";
    let date = parse_date_from_string(date_str);
    match date {
        Ok(d) => println!("Year: {}, Month: {}, Day: {}", d.year(), d.month(), d.day()),
        Err(e) => println!("Failed to parse date: {}", e),
    }
}
```

出力結果:

```
Year: 2022, Month: 3, Day: 11
```

## 深掘り：

**歴史的な文脈**：初期のプログラミング言語では、日付の解析は手間のかかる作業でした。しかし、現代の言語、特にRustでは、`chrono`のようなモジュールを活用してこの作業を容易に行えます。

**代替手段**：Rustには、`time`や`date`など、`chrono`以外の日付解析ライブラリも存在しますが、一般的に`chrono`が最もよく使用されるとされています。

**実装の詳細**：`parse_from_str`メソッドは、メソッドの引数として渡された文字列から日付を解析します。このメソッドは、日付形式を規定するフォーマットストリング（この例では"%Y-%m-%d"）も必要とします。

## 参照：

以下のリンクから、関連する情報源をご確認いただけます：

- Chrono Documentation: [https://docs.rs/chrono/0.4.19/chrono/](https://docs.rs/chrono/0.4.19/chrono/)
- Rust Documentation on Error Handling: [https://doc.rust-lang.org/book/ch09-00-error-handling.html](https://doc.rust-lang.org/book/ch09-00-error-handling.html)