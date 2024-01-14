---
title:                "Rust: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ
Rustは、安全性とパフォーマンスを両立したプログラミング言語です。そのため、日付の比較を行う際にも優れた選択肢となります。この記事では、Rustを使って日付を比較する方法についてご紹介します。

## 方法
まず、比較したい日付を`chrono`ライブラリを使用してDateTime型に変換します。次に、比較演算子（`<`,`<=`,`>`,`>=`,`==`,`!=`）を使って日付の大小を比較することができます。

例えば、2021年1月1日と2020年12月31日を比較したい場合、以下のようなコードを記述します。

```
use chrono::{DateTime, Local, Duration, Weekday, NaiveDate};
let first_date = Local.ymd(2021, 1, 1).and_hms(0, 0, 0);
let second_date = Local.ymd(2020, 12, 31).and_hms(0, 0, 0);

if first_date > second_date {
    println!("1月1日>12月31日");
} else if first_date < second_date {
    println!("1月1日<12月31日");
} else {
    println!("同じ日付です");
}
```

上記のコードを実行すると、コンソールには「1月1日>12月31日」というメッセージが表示されます。

## ディープダイブ
日付を比較する際には、タイムゾーンの影響を考慮することも重要です。Rustでは、タイムゾーンを指定してDateTime型を作成することができます。また、`chrono`ライブラリには、日付や時刻の計算を行うための多くの便利なメソッドが用意されています。

さらに、日付や時刻を表すデータ型には、`DateTime`以外にも`NaiveDate`や`NaiveDateTime`などがあり、それぞれの特徴や使い分けについても学ぶことができます。

## さらに見る
- [Rustの中の日付と時刻](https://doc.rust-lang.org/std/time/index.html)
- [Rustでのタイムゾーンの扱い](https://docs.rs/chrono/0.4.19/chrono/index.html)
- [パフォーマンス重視の日時比較方法](https://stackoverflow.com/questions/31427011/comparing-datetimes-in-rust)