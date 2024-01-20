---
title:                "未来または過去の日付を計算する"
html_title:           "Rust: 未来または過去の日付を計算する"
simple_title:         "未来または過去の日付を計算する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## 何となぜ？
「未来や過去の日付の計算」とは、現在日から特定の日数を加えたり減らしたりして別の日付を得ることです。これは予約システムなど、特定の期間後の日付を扱うプログラムで広く用いられます。

## 手順：
Rustではchronoパッケージを使って日付の計算が可能です。

Cargo.tomlに下記を追加します。

```Rust
[dependencies]
chrono = "0.4"
```

コード例：

```Rust
extern crate chrono;
use chrono::{DateTime, Duration, Utc};

fn main() {
    let now: DateTime<Utc> = Utc::now();
    println!("現在: {}", now);

    let two_weeks_from_now = now + Duration::weeks(2);
    println!("2週間後: {}", two_weeks_from_now);

    let two_weeks_ago = now - Duration::weeks(2);
    println!("2週間前: {}", two_weeks_ago);
}
```

出力結果:

```Rust
現在: 2022-05-20 12:34:56.7890 UTC
2週間後: 2022-06-03 12:34:56.7890 UTC
2週間前: 2022-05-06 12:34:56.7890 UTC
```

## ディープダイブ
このような日付の計算は、COBOLなどの古い言語では煩雑で困難でしたが、Rustのような新しい言語では大変簡単に行うことが可能です。Rustではchronoパッケージが慣用的に使われます。他のパッケージとしてはtimeパッケージがありますが、chronoの方が機能が充実しています。この計算の実装は、日数を単純に加減算しているだけでなく、うるう年などの日数のズレの調整も行っています。

## 参照情報
1. Chrono公式ドキュメンテーション： https://docs.rs/chrono/0.4.19/chrono/
2. Rustの日付と時間：https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html
3. Rustプログラミングの日付操作：https://qiita.com/mopp/items/7c6d5c0326af6e02504c