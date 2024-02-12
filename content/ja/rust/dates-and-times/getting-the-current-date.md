---
title:                "現在の日付の取得"
date:                  2024-02-03T19:10:58.526675-07:00
model:                 gpt-4-0125-preview
simple_title:         "現在の日付の取得"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
