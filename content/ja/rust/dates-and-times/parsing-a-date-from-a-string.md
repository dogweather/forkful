---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:26.929933-07:00
description: "\u65B9\u6CD5: Rust\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u306F\
  \u65E5\u4ED8\u89E3\u6790\u304C\u76F4\u63A5\u542B\u307E\u308C\u3066\u3044\u307E\u305B\
  \u3093\u304C\u3001\u5E83\u304F\u4F7F\u7528\u3055\u308C\u3066\u3044\u308B`chrono`\u30AF\
  \u30EC\u30FC\u30C8\u306F\u3001\u65E5\u4ED8\u3068\u6642\u9593\u306E\u64CD\u4F5C\u306B\
  \u5805\u7262\u306A\u30BD\u30EA\u30E5\u30FC\u30B7\u30E7\u30F3\u3092\u63D0\u4F9B\u3057\
  \u307E\u3059\u3002\u307E\u305A\u3001`Cargo.toml`\u306B`chrono`\u3092\u8FFD\u52A0\
  \u3057\u307E\u3059\uFF1A."
lastmod: '2024-03-13T22:44:41.837991-06:00'
model: gpt-4-0125-preview
summary: "Rust\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306B\u306F\u65E5\u4ED8\u89E3\
  \u6790\u304C\u76F4\u63A5\u542B\u307E\u308C\u3066\u3044\u307E\u305B\u3093\u304C\u3001\
  \u5E83\u304F\u4F7F\u7528\u3055\u308C\u3066\u3044\u308B`chrono`\u30AF\u30EC\u30FC\
  \u30C8\u306F\u3001\u65E5\u4ED8\u3068\u6642\u9593\u306E\u64CD\u4F5C\u306B\u5805\u7262\
  \u306A\u30BD\u30EA\u30E5\u30FC\u30B7\u30E7\u30F3\u3092\u63D0\u4F9B\u3057\u307E\u3059\
  \u3002\u307E\u305A\u3001`Cargo.toml`\u306B`chrono`\u3092\u8FFD\u52A0\u3057\u307E\
  \u3059\uFF1A."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

## 方法:


### Rustの標準ライブラリを使用する（`chrono`クレート）
Rust標準ライブラリには日付解析が直接含まれていませんが、広く使用されている`chrono`クレートは、日付と時間の操作に堅牢なソリューションを提供します。まず、`Cargo.toml`に`chrono`を追加します：

```toml
[dependencies]
chrono = "0.4"
```

次に、`chrono`を使用して日付文字列を`NaiveDate`オブジェクトに解析します：

```rust
extern crate chrono;
use chrono::NaiveDate;

fn main() {
    let date_str = "2023-04-01";
    let date = NaiveDate::parse_from_str(date_str, "%Y-%m-%d")
        .expect("日付の解析に失敗しました");

    println!("解析した日付: {}", date);
}

// サンプル出力:
// 解析した日付: 2023-04-01
```

### Rustの高度な日時処理を使用する（`time`クレート）
より高度な日時処理、包括的な解析を含む場合には、`time`クレートを検討します。まず、`Cargo.toml`にそれを含めます：

```toml
[dependencies]
time = "0.3"
```

次に、`Date`型と`PrimitiveDateTime`を使用して日付文字列を解析します：

```rust
use time::{Date, PrimitiveDateTime, macros::datetime};

fn main() {
    let date_str = "2023-04-01 12:34:56";
    let parsed_date = PrimitiveDateTime::parse(
        date_str, 
        &datetime!("%Y-%m-%d %H:%M:%S")
    ).expect("日付と時刻の解析に失敗しました");

    println!("解析した日時: {}", parsed_date);
}

// サンプル出力:
// 解析した日時: 2023-04-01 12:34:56
```

これらの例は、Rustがサードパーティのクレートを通じて、日付文字列を操作可能な日付オブジェクトに解析する手助けをする方法を示しており、時間的データを扱うソフトウェア開発において強力なツールであることを証明しています。
