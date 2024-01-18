---
title:                "文字列から日付を解析する"
html_title:           "Rust: 文字列から日付を解析する"
simple_title:         "文字列から日付を解析する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## パースするとは何か？ なぜプログラマーはそれをするのか？
パースするとは、テキストや文字列から日付を取り出すことを指します。プログラマーは、ユーザーから入力されたテキストを処理する必要があるため、日付を取得する必要があります。

## 方法：
```Rust
use chrono::DateTime;
use chrono::format::ParseError;
use chrono::offset::Utc;

fn parse_date(date_string: &str) -> Result<DateTime<Utc>, ParseError> {
    DateTime::parse_from_str(date_string, "%Y-%m-%d %H:%M:%S %z")
}
```

上記のコードは、与えられた文字列から日時を取得する方法を示しています。`DateTime`モジュールを使用し、`parse_from_str`関数に日付をフォーマットする方法を指定します。

**入力:**

`"1980-08-30 09:30:20 +0000"`

**出力:**

`1980-08-30T09:30:20Z`

## 一歩深く行く：
日付を文字列からパースする方法は、コンピューターの歴史の中でも重要な役割を果たしています。プログラミング言語によっては、内部的に`struct`や`class`を使用して日付を表現することもあります。また、パースの代替手段として、正規表現を使用することもできます。

## 関連リンク：
- [クロノライブラリ](https://github.com/chronotope/chrono)
- [Rustの正規表現チュートリアル](https://doc.rust-lang.org/rust-by-example/std/regex.html)