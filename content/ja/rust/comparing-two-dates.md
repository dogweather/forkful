---
title:                "Rust: 「2つの日付の比較」"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

プログラマーであるなら、日付を比較する機会は多いはずです。例えば、ある日付が別の日付よりも過去か未来かを判断する必要がある場合や、特定の日付範囲内にあるデータを取得する必要がある場合などです。Rustは、日付を比較する際に非常に強力であり正確な言語です。そのため、この記事ではRustを使って日付を比較する方法を紹介します。

## 方法

まずは、日付を比較するためには、`DateTime`ライブラリを使用する必要があります。次のように、プロジェクトに`DateTime`を追加します。

```Rust
[dependencies]
datetime = "0.4"
```

`DateTime`ライブラリを使用すると、日付オブジェクトを作成して比較することができます。例えば、2つの日付を比較するプログラムを以下のように記述することができます。

```Rust
extern crate datetime;

use datetime::{Date, Datelike};

fn main() {
    let date1 = Date::from_ymd(2021, 12, 25);
    let date2 = Date::from_ymd(2022, 1, 1);

    if date1 < date2 {
        println!("date1 is before date2");
    } else if date1 > date2 {
        println!("date1 is after date2");
    } else {
        println!("Both dates are the same");
    }
}
```

上記のコードを実行すると、出力結果は以下のようになります。

```
date1 is before date2
```

このように、`<`、`>`、`=`演算子を使用して日付を比較することができます。

また、日付を比較する際には、`chrono`ライブラリも便利です。`chrono`ライブラリを使用すると、日付をより詳細に操作することができます。例えば、以下のように使用することができます。

```Rust
extern crate chrono;

use chrono::prelude::*;

fn main() {
    let dt1 = NaiveDate::from_ymd(2021, 12, 25);
    let dt2 = NaiveDate::from_ymd(2022, 1, 1);

    if dt1 < dt2 {
        println!("dt1 is before dt2");
    } else if dt1 > dt2 {
        println!("dt1 is after dt2");
    } else {
        println!("Both dates are the same");
    }
}
```

出力結果は同じく、以下のようになります。

```
dt1 is before dt2
```

## 詳細

日付を比較する際には、注意しなければならない点があります。それは、日付オブジェクトに含まれるタイムゾーンの扱いです。Rustでは、デフォルトでローカルタイムゾーンが使用されるため、異なるタイムゾーンの日付を比較すると正確な結果が得られない場合があります。そのため、日付オブジェクトを比較する前に、必要に応じて同じタイムゾーンに変換する必要があります。

また、日付だけでなく、日付と時刻を比較することもできます。しかし、その場合はさらに注意が必要です。なぜなら、同じタイムゾーンの中でも、サマータイムやダブルサマータイムなどの影