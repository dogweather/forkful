---
title:                "「二つの日付の比較」"
html_title:           "Rust: 「二つの日付の比較」"
simple_title:         "「二つの日付の比較」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

＃＃それは何ですか？
日付を比較することは、2つの日付を比べて、どちらが早いかや同じ日付かを確認することです。プログラマーは、日付を比較することで、データを整理したり、特定の条件に基づいて処理を実行したりすることができます。

＃＃方法：
```Rust
use std::time::{SystemTime, UNIX_EPOCH}; // 時間を取得するためのライブラリをインポート
use chrono::{NaiveDate, Local}; // 日付を扱うためのライブラリをインポート

// 現在の日付を取得する
let now = SystemTime::now(); 

// UNIXエポックからの経過秒数を取得する
let since_epoch = now.duration_since(UNIX_EPOCH).expect("Time went backwards"); 

// 経過秒数を日付オブジェクトに変換する
let now_date = Local.timestamp(since_epoch.as_secs() as i64, 0); 

// 比較する日付を定義する
let date1 = NaiveDate::from_ymd(2021, 6, 20); 
let date2 = NaiveDate::from_ymd(2021, 6, 30); 

// 日付を比較し、結果を出力する
if date1 < date2 { 
    println!("日付 {} は、日付 {} よりも前にあります。", date1, date2);
} else if date1 > date2 {
    println!("日付 {} は、日付 {} よりも後ろにあります。", date1, date2);
} else {
    println!("日付 {} と日付 {} は同じです。", date1, date2);
}
```

出力：
日付 2021-06-20 は、日付 2021-06-30 よりも前にあります。

＃＃入門：
日付を比較する方法として、UNIXエポックという時刻の基準を使う方法があります。この基準は、1970年1月1日 00:00:00からの経過秒数を表し、各プログラミング言語で標準的な方法として使用されています。また、一部のライブラリを使用することで、より高度な日付の操作が可能になります。

＃＃詳細：
UNIXエポックは、プログラムで扱いやすいように、秒数単位の数値として表現されます。そのため、日付を比較する際には、まずUNIXエポックからの経過秒数を取得し、それを日付オブジェクトに変換する必要があります。また、Rustで日付を扱う場合は、多くのライブラリがありますが、代表的なものとしては「chrono」があります。

＃＃参考：
- [Rust公式ドキュメント](https://doc.rust-lang.org/nightly/std/time/struct.SystemTime.html)
- [UNIXエポックについての詳細な解説](https://www.epochconverter.com/)
- [chronoライブラリのドキュメント](https://docs.rs/chrono/0.4.19/chrono/)