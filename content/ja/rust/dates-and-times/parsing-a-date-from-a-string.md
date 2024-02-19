---
aliases:
- /ja/rust/parsing-a-date-from-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:26.929933-07:00
description: "\u2026"
lastmod: 2024-02-18 23:08:54.730101
model: gpt-4-0125-preview
summary: "\u2026"
title: "\u6587\u5B57\u5217\u304B\u3089\u65E5\u4ED8\u3092\u30D1\u30FC\u30B9\u3059\u308B"
---

{{< edit_this_page >}}

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
