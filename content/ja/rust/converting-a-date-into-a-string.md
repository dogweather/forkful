---
title:                "Rust: 日付を文字列に変換する"
programming_language: "Rust"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## なぜ
日付を文字列に変換することの意義や重要性については、まずその利便性から述べられます。例えば、データベースに日付を保存する場合や、ユーザーに特定の日付を表示する場合などに、日付を文字列に変換する必要があります。

## 方法
まずはRustの日付ライブラリであるchronoを利用します。次に、DateTimeと呼ばれるstructを使用して特定の日付を表します。その日付を文字列に変換するために、formatメソッドを使います。以下のコード例を参考にしてください。

```Rust
use chrono::{DateTime, TimeZone, Utc};

let now: DateTime<Utc> = Utc::now();
let date_string = now.format("%d/%m/%Y").to_string();
println!("{}", date_string)
```

出力:
```
12/05/2021
```

## 深堀り
日付を文字列に変換する際には、formatメソッドの引数に指定したフォーマットに従う必要があります。このフォーマットには、日付や時間を表記するための特定の文字列があります。例えば、"%d/%m/%Y"は日付を"DD/MM/YYYY"という形式で表すことを意味します。詳しくはchronoのドキュメントを確認してください。

## 参考リンク
- [chronoドキュメント](https://docs.rs/chrono/0.4.19/chrono/)
- [Rustにおける日付と時刻の取り扱い](https://qiita.com/huyuumi/items/bb8eba7efd1592905bc4)