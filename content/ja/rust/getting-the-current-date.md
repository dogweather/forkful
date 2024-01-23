---
title:                "現在の日付を取得する"
date:                  2024-01-20T15:16:47.296190-07:00
html_title:           "Bash: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? なにを、なぜ？
(1) 現在の日付を取得することは、システムの現在時刻を知るプロセスです。 (2) プログラマーはログ、ユーザーのアクティビティ、データのタイムスタンプを記録するために日付を利用します。

## How to:
Rustで現在の日付を取得するには、`chrono`クレートを使うのが一般的です。以下のステップに従いましょう。

1. `Cargo.toml`に`chrono`クレートを加える:
   ```toml
   [dependencies]
   chrono = "0.4"
   ```

2. コードで現在の日付を取得する:
   ```rust
   extern crate chrono;
   use chrono::{Local, Datelike};

   fn main() {
       let current_date = Local::today();
       println!("Current date: {}", current_date);
   }
   ```

3. 実行結果は次のようになります:
   ```
   Current date: YYYY-MM-DD
   ```

## Deep Dive
歴史的な背景: 日付と時刻の取り扱いはコンピューティングの初期から重要とされてきました。異なるシステム間での日付の表記方法の違いが多くあります。

代替手段: `chrono`以外にも、標準ライブラリの`std::time`モジュールを使用することで現在時刻を取得可能です。

実装の詳細: `chrono`クレートは多機能でタイムゾーンに対応しています。`Local::today()`はローカルタイムゾーンでの現在の日付を取得します。UTCを使いたい場合は`Utc::today()`が利用できます。

## See Also
- [The Rust Programming Language](https://doc.rust-lang.org/book/) - Rustの基本情報。
- [Chrono Crate Documentation](https://docs.rs/chrono/) - `chrono`クレートの詳細なドキュメント。
- [Rust by Example](https://doc.rust-lang.org/rust-by-example/) - 実際のコード例でRustを学ぶ。
