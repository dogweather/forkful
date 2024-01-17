---
title:                "「日付を文字列に変換する」"
html_title:           "Rust: 「日付を文字列に変換する」"
simple_title:         "「日付を文字列に変換する」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## これは何ですか？

日付を文字列に変換することは、プログラマーにとって重要です。日付を数字で表示したり、特定の形式で表示したりする必要があるためです。例えば、データベースに格納された日付をユーザーに分かりやすい形式で表示するときなどに使用されます。

## 方法：

以下のコード例を使用して、日付を文字列に変換する方法を説明します。

```Rust
use chrono::{Local, DateTime, TimeZone};

// 現在の日付を取得
let now = Local::now();

// 日付を文字列に変換
let string_date = now.format("%Y/%m/%d").to_string();

// 出力
println!("{}", string_date); // 例：2020/05/15
```

## 深堀り：

日付を文字列に変換する方法にはいくつかのアルゴリズムがありますが、Rustでは標準ライブラリの`chrono`パッケージを使用することができます。また、月や曜日などのローカライズ（言語や地域に応じた表記）にも対応しています。他にも、外部のライブラリを使用することでさまざまな方法で日付を文字列に変換することができます。

## 関連リンク：

- [Rust標準ライブラリのchronoパッケージのドキュメント](https://docs.rs/chrono)
- [Rustで日付を扱うためのライブラリの比較記事（英語）](https://nick.groenen.me/posts/rust-datetime-libraries/)
- [ローカライズ対応の例 - chronoパッケージのドキュメント（英語）](https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html#example-localization)