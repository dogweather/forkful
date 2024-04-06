---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:21.347753-07:00
description: "\u4F7F\u3044\u65B9\uFF1A Rust\u306E`regex`\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306F\u3001\u6B63\u898F\u8868\u73FE\u3092\u4F7F\u3046\u969B\u306E\u4E3B\u8981\u306A\
  \u30C4\u30FC\u30EB\u3067\u3059\u3002\u3053\u308C\u3092\u4F7F\u7528\u3059\u308B\u306B\
  \u306F\u3001\u307E\u305A`Cargo.toml`\u3078\u306E\u8FFD\u52A0\u304C\u5FC5\u8981\u3067\
  \u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.711629-06:00'
model: gpt-4-0125-preview
summary: ''
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

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
