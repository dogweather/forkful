---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:58.526675-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.838803-06:00'
model: gpt-4-0125-preview
summary: "Rust\u3067\u73FE\u5728\u306E\u65E5\u4ED8\u3092\u53D6\u5F97\u3059\u308B\u3053\
  \u3068\u306F\u3001\u30ED\u30B0\u53D6\u308A\u3001\u6642\u9593\u30D9\u30FC\u30B9\u306E\
  \u64CD\u4F5C\u3001\u3042\u308B\u3044\u306F\u5358\u306B\u65E5\u4ED8\u3092\u8868\u793A\
  \u3059\u308B\u3068\u3044\u3063\u305F\u30BF\u30B9\u30AF\u306B\u3068\u3063\u3066\u4E00\
  \u822C\u7684\u306A\u4F5C\u696D\u3067\u3059\u3002\u4E00\u90E8\u306E\u8A00\u8A9E\u304C\
  \u305D\u306E\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u65E5\u4ED8\u3068\u6642\
  \u9593\u306E\u6A5F\u80FD\u3092\u542B\u3080\u306E\u3068\u306F\u7570\u306A\u308A\u3001\
  Rust\u306F\u65E5\u4ED8\u3068\u6642\u9593\u306E\u64CD\u4F5C\u306B\u95A2\u3057\u3066\
  \u5305\u62EC\u7684\u306A\u6A5F\u80FD\u3068\u4F7F\u3044\u3084\u3059\u3055\u306E\u305F\
  \u3081\u306B\u3001\u5805\u7262\u306A\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u30E9\
  \u30A4\u30D6\u30E9\u30EA\u3067\u3042\u308Bchrono\u306E\u4F7F\u7528\u3092\u63A8\u5968\
  \u3057\u3066\u3044\u307E\u3059\u3002."
title: "\u73FE\u5728\u306E\u65E5\u4ED8\u306E\u53D6\u5F97"
weight: 29
---

## 何となぜ？

Rustで現在の日付を取得することは、ログ取り、時間ベースの操作、あるいは単に日付を表示するといったタスクにとって一般的な作業です。一部の言語がその標準ライブラリに日付と時間の機能を含むのとは異なり、Rustは日付と時間の操作に関して包括的な機能と使いやすさのために、堅牢なサードパーティライブラリであるchronoの使用を推奨しています。

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
