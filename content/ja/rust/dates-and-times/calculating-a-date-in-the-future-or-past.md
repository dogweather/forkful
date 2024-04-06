---
date: 2024-01-20 17:32:05.219397-07:00
description: ''
isCJKLanguage: true
lastmod: '2024-04-05T21:59:54.188896-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u6614\u306F\u30AB\u30EC\u30F3\u30C0\u30FC\u8A08\u7B97\
  \u304C\u5927\u5909\u3060\u3063\u305F\u3002\u3060\u3051\u3069\u3001\u30B3\u30F3\u30D4\
  \u30E5\u30FC\u30BF\u3068\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u51FA\u73FE\u3067\u7C21\
  \u5358\u306B\u306A\u3063\u305F\u3002Rust\u3067\u306F`chrono`\u30AF\u30EC\u30FC\u30C8\
  \u304C\u4EBA\u6C17\u3002\u4EE3\u66FF\u3068\u3057\u3066`time`\u30AF\u30EC\u30FC\u30C8\
  \u3082\u3042\u308B\u3002`chrono`\u306FTimezone\u306B\u3082\u5BFE\u5FDC\u3057\u3066\
  \u3044\u3066\u3001\u8A08\u7B97\u3082Intuitive\u3002\u5185\u90E8\u7684\u306B\u306F\
  \u3001\u30A8\u30DD\u30C3\u30AF\uFF081970\u5E741\u67081\u65E5\uFF09\u304B\u3089\u306E\
  \u7D4C\u904E\u6642\u9593\u3092\u30D9\u30FC\u30B9\u306B\u8A08\u7B97\u3059\u308B\u3088\
  \u306D\u3002"
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
