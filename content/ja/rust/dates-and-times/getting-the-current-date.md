---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:58.526675-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Rust\u306E\u6A19\u6E96\u30E9\u30A4\
  \u30D6\u30E9\u30EA\u306F\u3001\u73FE\u5728\u306E\u6642\u9593\u3092\u53D6\u5F97\u3059\
  \u308B\u305F\u3081\u306E\u9650\u5B9A\u7684\u3060\u304C\u8FC5\u901F\u306A\u65B9\u6CD5\
  \u3092\u63D0\u4F9B\u3057\u307E\u3059\u304C\u3001\u76F4\u63A5\u30AB\u30EC\u30F3\u30C0\
  \u30FC\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306E\u73FE\u5728\u306E\u65E5\u4ED8\u3092\
  \u53D6\u5F97\u3059\u308B\u308F\u3051\u3067\u306F\u3042\u308A\u307E\u305B\u3093\u3002\
  \u4EE5\u4E0B\u306E\u65B9\u6CD5\u3067\u884C\u3044\u307E\u3059\uFF1A."
lastmod: '2024-04-05T22:37:50.121895-06:00'
model: gpt-4-0125-preview
summary: "\u3069\u3046\u3084\u3063\u3066\uFF1A Rust\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\
  \u30E9\u30EA\u306F\u3001\u73FE\u5728\u306E\u6642\u9593\u3092\u53D6\u5F97\u3059\u308B\
  \u305F\u3081\u306E\u9650\u5B9A\u7684\u3060\u304C\u8FC5\u901F\u306A\u65B9\u6CD5\u3092\
  \u63D0\u4F9B\u3057\u307E\u3059\u304C\u3001\u76F4\u63A5\u30AB\u30EC\u30F3\u30C0\u30FC\
  \u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u306E\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\
  \u5F97\u3059\u308B\u308F\u3051\u3067\u306F\u3042\u308A\u307E\u305B\u3093\u3002\u4EE5\
  \u4E0B\u306E\u65B9\u6CD5\u3067\u884C\u3044\u307E\u3059\uFF1A."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## どうやって：


### Rustの標準ライブラリを使う
Rustの標準ライブラリは、現在の時間を取得するための限定的だが迅速な方法を提供しますが、直接カレンダーフォーマットの現在の日付を取得するわけではありません。以下の方法で行います：

```rust
use std::time::{SystemTime, UNIX_EPOCH};

fn main() {
    match SystemTime::now().duration_since(UNIX_EPOCH) {
        Ok(n) => println!("現在の時間: Unix エポックから {} 秒です。", n.as_secs()),
        Err(_) => panic!("システム時間がUnixエポックより前です！"),
    }
}
```

出力:
```
現在の時間: Unix エポックから 1615390665 秒です。
```

### Chronoライブラリを使う
より包括的な日付と時間の機能を得るためには、`chrono`ライブラリを使用すべきです。まず、`Cargo.toml`に`chrono`を追加します：

```toml
[dependencies]
chrono = "0.4"
```

次に、`chrono`を使って現在の日付を取得します：

```rust
extern crate chrono;
use chrono::{Local, Datelike};

fn main() {
    let now = Local::now();
    println!("現在の日付: {}-{}-{}", now.year(), now.month(), now.day());
}
```

出力:
```
現在の日付: 2023-4-20
```

`chrono` ライブラリを使用することで、日付と時間を扱うことが非常に単純になり、現在の日付を取得するだけでなく、日付と時間の解析、フォーマット、及び算術操作など、幅広い機能を提供します。
