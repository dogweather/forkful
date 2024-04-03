---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:26.929933-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.837991-06:00'
model: gpt-4-0125-preview
summary: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u89E3\u6790\u3059\u308B\
  \u306E\u306F\u3001\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u3092\u6271\u3046\u5834\u5408\
  \u3084\u30D5\u30A1\u30A4\u30EB\u304B\u3089\u306E\u30C7\u30FC\u30BF\u8AAD\u307F\u53D6\
  \u308A\u6642\u306B\u3088\u304F\u3042\u308B\u4F5C\u696D\u3067\u3059\u3002\u3053\u308C\
  \u306B\u306F\u3001\u6587\u5B57\u5217\u30C7\u30FC\u30BF\u3092\u30D7\u30ED\u30B0\u30E9\
  \u30DF\u30F3\u30B0\u8A00\u8A9E\u3067\u8A8D\u8B58\u3055\u308C\u308B\u65E5\u4ED8\u5F62\
  \u5F0F\u306B\u5909\u63DB\u3059\u308B\u4F5C\u696D\u304C\u542B\u307E\u308C\u307E\u3059\
  \u3002Rust\u3067\u306F\u3001\u6BD4\u8F03\u3001\u7B97\u8853\u51E6\u7406\u3001\u307E\
  \u305F\u306F\u30D5\u30A9\u30FC\u30DE\u30C3\u30C6\u30A3\u30F3\u30B0\u306A\u3069\u306E\
  \u65E5\u4ED8\u64CD\u4F5C\u306B\u4E0D\u53EF\u6B20\u3067\u3042\u308A\u3001\u30A2\u30D7\
  \u30EA\u30B1\u30FC\u30B7\u30E7\u30F3\u306E\u30C7\u30FC\u30BF\u691C\u8A3C\u3068\u6574\
  \u5408\u6027\u3092\u9AD8\u3081\u308B\u306E\u306B\u5F79\u7ACB\u3061\u307E\u3059\u3002\
  ."
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
weight: 30
---

## 何となぜ？

文字列から日付を解析するのは、ユーザー入力を扱う場合やファイルからのデータ読み取り時によくある作業です。これには、文字列データをプログラミング言語で認識される日付形式に変換する作業が含まれます。Rustでは、比較、算術処理、またはフォーマッティングなどの日付操作に不可欠であり、アプリケーションのデータ検証と整合性を高めるのに役立ちます。

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
