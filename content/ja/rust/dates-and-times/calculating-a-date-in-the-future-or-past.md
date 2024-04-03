---
date: 2024-01-20 17:32:05.219397-07:00
description: "How to: (\u3084\u308A\u65B9) ."
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.842828-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
weight: 26
---

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
