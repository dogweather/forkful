---
title:                "Rust: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Rustで日付を文字列に変換する方法

## Why (なぜ)
日付をプログラム内で扱う際には、文字列としてフォーマットする必要がある場合があります。Rustでは強力なデータ型を持つため、日付を文字列に変換する際も簡単に行うことができます。

## How To (方法)
日付を文字列に変換するには、[`to_string()`](https://doc.rust-lang.org/std/primitive.str.html#method.to_string)メソッドを使用します。このメソッドは`DateTime`型から`String`型への変換を行います。

```Rust
use chrono::{DateTime, Local, Utc};

fn main() {
    // 現在時刻を取得
    let now = Local::now();
    // UTCの日付を生成
    let utc_date = DateTime::<Utc>::from_utc(now.naive_utc(), Utc);
    
    // フォーマットを指定して文字列に変換
    let date_string = utc_date.to_string(); 
    println!("Date as String: {}", date_string); // Date as String: 2020-09-26 03:07:11
    
    // 別のフォーマットで文字列に変換
    let custom_format = utc_date.format("%Y-%m-%d"); 
    println!("Custom Format: {}", custom_format); // Custom Format: 2020-09-26
}
```

## Deep Dive (詳細)
日付を文字列に変換するためには、[`format!()`](https://doc.rust-lang.org/std/macro.format.html)マクロを使用することもできます。このマクロを使用すると、[`DateTimeFormatter`](https://docs.rs/chrono/0.4.19/chrono/format/struct.DateTimeFormatter.html)オブジェクトを生成し、さまざまなフォーマットを定義することができます。

```Rust
use chrono::{DateTime, Local, Utc, DateTimeFormatter, TimeZone};

fn main() {
    // 日付を生成
    let date = Utc::now();
    
    // DateTimeFormatterの生成
    let formatter = DateTimeFormatter::new("%a %b %e %T.%f %Y %Z"); 
    
    // フォーマットした文字列を取得
    let date_string = formatter.format(&date); 
    println!("Custom Format: {}", date_string); // Custom Format: Sat Sep 26 03:24:17.548196 2020 UTC
}
```

## See Also (関連情報)
- [`chrono`ライブラリドキュメンテーション](https://docs.rs/chrono/0.4.19/chrono/)
- [Rustプログラミング言語公式ドキュメント](https://doc.rust-jp.rs/book-ja/)