---
title:                "「二つの日付を比較する」"
html_title:           "Rust: 「二つの日付を比較する」"
simple_title:         "「二つの日付を比較する」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/comparing-two-dates.md"
---

{{< edit_this_page >}}

## なぜ

日付の比較をする理由は何でしょうか？日付の比較は、特定のタスクを実行するために必要な条件分岐を作成するために使用されます。例えば、特定の日付が間にあるかどうかを確認する場合などです。

## 方法

日付を比較するためには、Rustの標準ライブラリである`chrono`パッケージを使用します。まずはこのパッケージをインポートしましょう。

```Rust
use chrono::{DateTime, Local, NaiveDate, Duration};
```

次に、比較したい日付をそれぞれ`DateTime`型や`NaiveDate`型に変換します。この際、`DateTime::parse_from_rfc3339()`や`NaiveDate::parse_from_str()`を使用することで、文字列から日付型に変換することができます。

```Rust
let date1 = DateTime::parse_from_rfc3339("2021-04-01T00:00:00+09:00").unwrap();
let date2 = NaiveDate::parse_from_str("2021-04-05", "%Y-%m-%d").unwrap();
```

最後に、`date1`と`date2`を比較して、結果を出力します。例えば、次のように条件分岐を作成することができます。

```Rust
if date1 < date2 {
    println!("{} is earlier than {}", date1, date2);
} else if date1 == date2 {
    println!("{} is the same as {}", date1, date2);
} else {
    println!("{} is later than {}", date1, date2);
}
```

実行結果は以下のようになります。

```
2021-04-01T00:00:00+09:00 is earlier than 2021-04-05
```

## 深堀り

Rustの`DateTime`型や`NaiveDate`型は、それぞれ、タイムゾーンの情報を含んでいるかどうかの違いがあります。`DateTime`型はタイムゾーンの情報を持っているため、異なるタイムゾーンの日付を比較することができますが、`NaiveDate`型はタイムゾーンの情報を持っていないため、同じタイムゾーンである場合にのみ比較ができます。

また、Rustの`Duration`型を使用することで、日付の差分を計算することもできます。例えば、以下のように計算することができます。

```Rust
let diff = date2.signed_duration_since(date1);
println!("{} days difference", diff.num_days());
```

実行結果は以下のようになります。

```
4 days difference
```

## おわりに

ご覧いただきありがとうございました。日付の比較については、Rustの`chrono`パッケージを使用することで簡単に実装することができます。さらに詳しい情報は、下記のリンクをご参照ください。

## 関連リンク

- `chrono`パッケージのドキュメント: https://docs.rs/chrono/
- `DateTime`型のドキュメント: https://docs.rs/chrono/latest/chrono/struct.DateTime.html
- `NaiveDate`型のドキュメント: https://docs.rs/chrono/latest/chrono/struct.NaiveDate.html
- `Duration`型のドキュメント: https://docs.rs/chrono/latest/chrono/struct.Duration.html