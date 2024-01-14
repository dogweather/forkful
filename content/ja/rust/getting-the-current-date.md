---
title:    "Rust: 現在の日付を取得する"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/getting-the-current-date.md"
---

{{< edit_this_page >}}

## なぜ

現在の日付を取得することの重要性を説明するために、 この Rust プログラミングのブログ記事があります。

## 方法

日付を取得するには、標準ライブラリの `chrono` モジュールを使用します。まずは `Local` というタイムゾーンを指定します。

```Rust
use chrono::{Local, Datelike};

let local = Local::today();
```

その後、 `Local` のメソッドを使用することで、現在の日付や特定の要素を取得することができます。

```Rust
// 現在の日付を取得
let today = local.date();

// 年を取得
let year = local.year();

// 月を取得
let month = local.month();

// 日を取得
let day = local.day();

// 曜日を取得
let weekday = local.weekday();
```

また、指定した形式に日付をフォーマットすることもできます。

```Rust
// "YYYY-MM-DD" 形式でフォーマット
let formatted_date = today.format("%Y-%m-%d");
```

下記がサンプルコードの出力結果になります。

```
年：2021
月：12
日：31
曜日：金
フォーマットした日付：2021-12-31
```

## ディープダイブ

`chrono` モジュールは、より複雑な日付操作を行うことも可能です。例えば、日付の比較や加算、減算、フォーマット、タイムゾーンの変更などができます。

また、`NaiveDate` や `DateTime` といった、より詳細な日付オブジェクトを使用することもできます。詳細な情報は [公式ドキュメント](https://docs.rs/chrono/) を参照してください。

## 参考文献

- [Rust プログラミング言語公式サイト](https://www.rust-lang.org/ja/)
- [chrono モジュールのドキュメント](https://docs.rs/chrono/)
- [Rust での日付操作のためのライブラリ一覧](https://lib.rs/date)
- [Rust の標準ライブラリのドキュメント (chrono は `time` モジュールに含まれます)](https://doc.rust-lang.org/std/time/index.html)

## 関連リンク

- [Rust の日付操作に関する質問/回答サイト (Stack Overflow)](https://stackoverflow.com/questions/tagged/rust+datetime)
- [Rust における時刻と時間に関する知識 (Qiita)](https://qiita.com/enechange/items/3918008d6a399d8a2751)
- [Rust での日付操作のベストプラクティス (Medium)](https://medium.com/better-programming/a-guide-to-working-with-dates-time-in-rust-750acdfe88a9)