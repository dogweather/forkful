---
title:                "将来または過去の日付を計算する"
aliases: - /ja/rust/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:32:05.219397-07:00
model:                 gpt-4-1106-preview
simple_title:         "将来または過去の日付を計算する"

tag:                  "Dates and Times"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
将来または過去の日付を計算するって、ある日から特定の日数を足したり引いたりすることだよ。何のために使うかって？イベント予定を立てたり、期限を計算したりする時に便利だからさ。

## How to: (やり方)
```Rust
use chrono::{Duration, Utc};

fn main() {
    let today = Utc::now();
    println!("Today: {}", today);

    let future_date = today + Duration::days(30);
    println!("Future date: {}", future_date);

    let past_date = today - Duration::days(30);
    println!("Past date: {}", past_date);
}
```

出力例：
```
Today: 2023-04-14T12:00:00Z
Future date: 2023-05-14T12:00:00Z
Past date: 2023-03-15T12:00:00Z
```

## Deep Dive (掘り下げ)
昔はカレンダー計算が大変だった。だけど、コンピュータとライブラリの出現で簡単になった。Rustでは`chrono`クレートが人気。代替として`time`クレートもある。`chrono`はTimezoneにも対応していて、計算もIntuitive。内部的には、エポック（1970年1月1日）からの経過時間をベースに計算するよね。

## See Also (関連情報)
- [The Rust Programming Language](https://doc.rust-lang.org/book/) - Rustの基本を学ぶための資料。
