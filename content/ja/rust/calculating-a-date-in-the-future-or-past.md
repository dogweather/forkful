---
title:                "Rust: 将来または過去の日付の計算"
simple_title:         "将来または過去の日付の計算"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## なぜ

日付の将来または過去を計算する理由は、よく使われるプログラミングのタスクの1つです。例えば、予定されたイベントの日付を特定するために、または繰り返し発生するタスクの日付を自動的に決定するために使用されます。Rustで日付を計算する方法を学ぶと、より効率的に作業できるようになります。

## 方法

```Rust
use chrono::{NaiveDate, Duration};

// 過去の日付から3ヶ月前の日付を取得する
let past_date = NaiveDate::from_ymd(2020, 5, 10);
let future_date = past_date - Duration::months(3);

// どの日付フォーマットを使っても、Rustは自動的に日付をパースします。
println!("過去の日付から3ヶ月前の日付は: {}", future_date.format("%Y-%m-%d"));
```

上記のコードを実行すると、以下の出力が得られます。
```
過去の日付から3ヶ月前の日付は: 2020-02-10
```

さらに、日付を計算するために使用できる他の便利なメソッドには、`Duration::days()`（日数を指定して日付を計算）や`DateTime::with_month()`（日付を特定の月に変更）などがあります。

## ディープダイブ

日付を計算するためのRustの最も重要なライブラリは、`chrono`です。`chrono`は、日付や時間を表すための様々な型を提供し、日付のパースやフォーマットに便利なメソッドを提供します。また、`Duration`型を使用すると、日付の加算や減算が簡単になります。

また、Rustの他のライブラリやフレームワークでも日付の計算を行うことができます。例えば、`rust-date`は、日付や時間の操作をより直感的に行うことができるように設計されています。

## その他の参考リンク

- [Rustの日付・時刻ライブラリ「chrono」の基本的な使い方まとめ](https://qiita.com/rgbkids/items/4f91a2574442a4306a4f)
- [Rustで日付を加算、減算する方法](https://github.com/m1ch1/chrono/blob/master/examples/add_sub.rs)
- [Rustで日付を操作するための他のライブラリやフレームワークの一覧](https://github.com/rust-unofficial/awesome-rust#date-and-time)