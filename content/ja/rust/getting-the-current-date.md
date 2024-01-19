---
title:                "現在の日付を取得する"
html_title:           "PowerShell: 現在の日付を取得する"
simple_title:         "現在の日付を取得する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## 何 &なぜ?
現在の日付を取得するとは、コンピュータの内部時計から年月日を取り出すことです。これはプログラムが時制を管理したり、時間に基づいた制御を行ったりするために重要です。

## 実行方法:
Rustには`chrono`という、非常に便利で使いやすい日付と時間のライブラリがあります。以下にその使用例を示します。

```Rust
// 先にCargo.tomlにchronoライブラリを追加してください
extern crate chrono;
use chrono::prelude::*;

fn main() {
    let now = Utc::now();
    println!("{}", now.format("%Y-%m-%d").to_string());
}
```

これを実行すると、今日の日付が年-月-日の形式で表示されます。

## 詳細について:
### 歴史的背景
UNIXエポック (1970年1月1日の午前0時 UTC) からの経過時間という考え方は、これまでのコンピュータ科学にとって非常に重要でした。だからこそ、日時を扱うためのダイナミックな方法を持つRustのようなモダンな言語は、非常に役立つと言えるでしょう。

### 代替案
Rustには、日付と時間を扱うための他のライブラリもあります。`time`、`date-time`、`naive-datetime`などがそれに該当します。

### 実装の詳細
RustのDateTime型は、ナノ秒単位での精度を持つことができます。また、`chrono`ライブラリはtime zoneにも対応しており、グローバルなコンテキストで使うのに適しています。

## 参考資料:
- Chronoライブラリの公式ドキュメンテーション（英語） [Chrono](https://docs.rs/chrono/0.4.19/chrono/)
- Rustの日付と時刻についての公式ドキュメンテーション（英語） [Date and Time](https://doc.rust-lang.org/std/time/)