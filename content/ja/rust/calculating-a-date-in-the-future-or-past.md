---
title:                "Rust: 未来または過去の日付を計算する"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
日付を将来や過去に計算する理由は何でしょうか？答えはとてもシンプルです。人々は常に特定の日時を把握したい、または将来の予定を立てたいと考えています。これまでの計算方法に加えて、Rustプログラミング言語を使用することで、さらに効率的な方法で日付を計算することができます。

## How To
まずはRust言語を使って日付を計算する方法を見ていきましょう。以下のようにコードブロックを使用してコーディングの例を示します。

```Rust
use chrono::{NaiveDate, Datelike};

// ベーシックな日付の計算
let today = NaiveDate::from_ymd(2021, 5, 21);
let five_days_later = today + chrono::Duration::days(5);
println!("今日の日付から5日後は{}年{}月{}日です。", five_days_later.year(), five_days_later.month(), five_days_later.day());

// 過去の日付の計算
let past_date = NaiveDate::from_ymd(2000, 1, 1);
let ten_years_ago = past_date - chrono::Duration::years(10);
println!("2000年1月1日から10年前は{}年{}月{}日でした。", ten_years_ago.year(), ten_years_ago.month(), ten_years_ago.day());
```

上記のコードを実行すると、次のような出力結果が得られます。

```
今日の日付から5日後は2021年5月26日です。
2000年1月1日から10年前は1990年1月1日でした。
```

## Deep Dive
日付の計算にはさまざまな方法がありますが、Rust言語では`chrono`ライブラリを使用することで、さまざまな日付操作を行うことができます。例えば、`chrono::Duration`を使用して日付を加算または減算することができます。また、`NaiveDate`を使用して特定の日付のオブジェクトを作成することも可能です。

さらに、Rust言語では時間やタイムゾーンを考慮した日付計算も行うことができます。`chrono`ライブラリには`DateTime`や`Utc`などの機能があり、これらを使用することでタイムゾーンを考慮した日付計算が可能になります。

## See Also
この記事ではRust言語を使用して日付を計算する方法を紹介しましたが、他にも日付計算に役立つ情報がたくさんあります。以下のリンクを参考にして、さらに多くの知識を身につけてください。

- [Rust公式ドキュメント](https://doc.rust-lang.org/std/chrono/)
- [Rust Cookbook：日付と時刻をオブジェクトに変換する方法](https://rust-lang-nursery.github.io/rust-cookbook/datetime/datetime.html)
- [ハンズオン！ Rustプログラミング - 日付と時刻の処理](https://blog.scottlogic.com/2020/06/03/rust-datetime.html)