---
aliases:
- /ja/rust/calculating-a-date-in-the-future-or-past/
date: 2024-01-20 17:32:05.219397-07:00
description: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3063\u3066\u3001\u3042\u308B\u65E5\u304B\u3089\u7279\u5B9A\u306E\
  \u65E5\u6570\u3092\u8DB3\u3057\u305F\u308A\u5F15\u3044\u305F\u308A\u3059\u308B\u3053\
  \u3068\u3060\u3088\u3002\u4F55\u306E\u305F\u3081\u306B\u4F7F\u3046\u304B\u3063\u3066\
  \uFF1F\u30A4\u30D9\u30F3\u30C8\u4E88\u5B9A\u3092\u7ACB\u3066\u305F\u308A\u3001\u671F\
  \u9650\u3092\u8A08\u7B97\u3057\u305F\u308A\u3059\u308B\u6642\u306B\u4FBF\u5229\u3060\
  \u304B\u3089\u3055\u3002"
isCJKLanguage: true
lastmod: 2024-02-18 23:08:54.733496
model: gpt-4-1106-preview
summary: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\
  \u7B97\u3059\u308B\u3063\u3066\u3001\u3042\u308B\u65E5\u304B\u3089\u7279\u5B9A\u306E\
  \u65E5\u6570\u3092\u8DB3\u3057\u305F\u308A\u5F15\u3044\u305F\u308A\u3059\u308B\u3053\
  \u3068\u3060\u3088\u3002\u4F55\u306E\u305F\u3081\u306B\u4F7F\u3046\u304B\u3063\u3066\
  \uFF1F\u30A4\u30D9\u30F3\u30C8\u4E88\u5B9A\u3092\u7ACB\u3066\u305F\u308A\u3001\u671F\
  \u9650\u3092\u8A08\u7B97\u3057\u305F\u308A\u3059\u308B\u6642\u306B\u4FBF\u5229\u3060\
  \u304B\u3089\u3055\u3002"
title: "\u5C06\u6765\u307E\u305F\u306F\u904E\u53BB\u306E\u65E5\u4ED8\u3092\u8A08\u7B97\
  \u3059\u308B"
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
