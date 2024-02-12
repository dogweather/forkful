---
title:                "正規表現の使用"
aliases:
- /ja/rust/using-regular-expressions/
date:                  2024-02-03T19:18:21.347753-07:00
model:                 gpt-4-0125-preview
simple_title:         "正規表現の使用"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 何となく、なぜ？

正規表現（regex）を使うと、開発者は文字列の検索、一致、そして高度なパターンマッチング技術での操作が可能になります。Rustでのregexの利用は、テキストデータの解析と処理を効率的に行うのに役立ち、データ検証、検索、テキスト変換などのタスクをより簡潔かつ保守しやすくします。

## 使い方：

Rustの`regex`ライブラリは、正規表現を使う際の主要なツールです。これを使用するには、まず`Cargo.toml`への追加が必要です：

```toml
[dependencies]
regex = "1"
```

その後、Rustのコードでregexの機能を実装することができます。ここでは、いくつかの一般的な操作を行う方法を示します：

### 文字列内のパターンをマッチングする

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
    let date = "2023-04-15";

    println!("Does the text match the date pattern? {}", re.is_match(date));
    // 出力：Does the text match the date pattern? true
}
```

### マッチを見つけてアクセスする

```rust
use regex::Regex;

fn main() {
    let text = "Rust 2023, C++ 2022, Python 2021";
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();

    for cap in re.captures_iter(text) {
        println!("Language: {}, Year: {}", &cap[1], &cap[2]);
    }
    // 出力：
    // Language: Rust, Year: 2023
    // Language: C++, Year: 2022
    // Language: Python, Year: 2021
}
```

### テキストを置換する

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b(\w+)\s(\d{4})").unwrap();
    let text = "Rust 2023, C++ 2022, Python 2021";
    let replaced = re.replace_all(text, "$1 was updated in $2");

    println!("Updated text: {}", replaced);
    // 出力: Updated text: Rust was updated in 2023, C++ was updated in 2022, Python was updated in 2021
}
```

### 正規表現を使用してテキストを分割する

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\W+").unwrap(); // 単語でない文字で分割
    let text = "Rust-C++-Python-Go";

    let fields: Vec<&str> = re.split(text).collect();

    for field in fields {
        println!("Language: {}", field);
    }
    // 出力：
    // Language: Rust
    // Language: C++
    // Language: Python
    // Language: Go
}
```

これらの例は、Rustで正規表現を使い始めるための基本的なガイドを提供します。より複雑なパターンマッチングやテキスト操作のタスクが必要になった場合、`regex`クレートは豊富な機能を提供します。
