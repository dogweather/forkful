---
title:                "文字列から日付を解析する"
date:                  2024-01-20T15:38:32.951831-07:00
html_title:           "Arduino: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
StringからDateへのパース（解析）とは、日付の形式の文字列をプログラムで使えるDate型へと変換することです。データのバリデーションや形式の統一、日付の計算に不可欠だからです。

## How to: (やり方)
Rustでは、`chrono`というクレートを使って簡単に日付のパースができます。

```Rust
use chrono::{NaiveDate, ParseError};

fn parse_date_from_string(date_str: &str) -> Result<NaiveDate, ParseError> {
    NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
}

fn main() {
    let date_string = "2023-03-14";
    match parse_date_from_string(date_string) {
        Ok(date) => println!("Parsed date: {}", date),
        Err(e) => println!("Error parsing date: {}", e),
    }
}
```

実行結果:
```
Parsed date: 2023-03-14
```

## Deep Dive (深堀り)
日付のパースは多くのプログラミング言語で一般的な機能ですが、Rustでは型安全性とエラーハンドリングに重点を置いています。 `chrono`クレートはRustコミュニティで広く使われており、多様な日付・時間関連のニーズに対応しています。他の言語や旧バージョンのRustでは標準ライブラリだけで日付のパース機能を提供していたものもあれど、Rustでは外部クレートがその役割を果たしています。 `chrono`以外にも`time`クレートなどがありますが、機能や使いやすさに違いがあるため、プロジェクトに合わせて選ぶことが大切です。

## See Also (関連情報)
- [Rustの公式ドキュメント](https://doc.rust-lang.org/std/time/)
