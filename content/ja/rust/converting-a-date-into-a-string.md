---
title:                "日付を文字列に変換する"
html_title:           "C++: 日付を文字列に変換する"
simple_title:         "日付を文字列に変換する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Rustプログラミング：日付を文字列に変換する

## 何となぜ？
日付を文字列に変換するとは、日付データを独特な形式のテキスト、つまり文字列に変換することを指します。これは一般に、日付データをユーザーにわかりやすく表示するためや、特定のフォーマットでデータを保存・転送するために行われます。

## 実装方法：
Rustで日付を文字列に変換する基本的な方法を示します。

```Rust
use chrono::{Utc, DateTime};
let now: DateTime<Utc> = Utc::now();
let formatted = now.format("%Y-%m-%d %H:%M:%S").to_string();
println!("{}", formatted);
```

実行すると、現在の日付と時間が "2022-12-20 21:13:20" のような形式で表示されます。

## ディープダイブ
日付の扱いはプログラミングの歴史上、常に複雑な課題の一つであり、多くのプログラミング言語がそれらを独自に扱う方法を提供してきました。Rustもその例外ではありません。Rustには日付と時間を表現するための素晴らしいライブラリ`chrono`がありますが、時々直接扱う方が便利なケースもあります。

日付を操作する別の方法として、あなたが特定のフォーマットを求める場合、手動で日付データを文字列に変換することも可能です。しかしこのアプローチは、間違いの余地があり、実行するたびに異なる結果をもたらす可能性があるため、注意が必要です。

Rustの日付を文字列に変換する実装の詳細については、主に`format`メソッドが関与しています。このメソッドは、指定されたフォーマットで日時を文字列に変換します。フォーマット指定子は[公式ドキュメンテーション](https://docs.rs/chrono/0.4.19/chrono/format/strftime/index.html)で確認できます。

## 参考資料：
- [`chrono` crate 公式ドキュメンテーション](https://docs.rs/chrono/0.4.19/chrono/)
- [Rust by Example: Formatted print](https://doc.rust-lang.org/rust-by-example/hello/print/fmt.html)
- [Rust公式日付＆時間操作ガイド](https://docs.rs/chrono/0.4.19/chrono/)