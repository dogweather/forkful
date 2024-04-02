---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:18:21.347753-07:00
description: "\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u3092\u4F7F\u3046\u3068\u3001\
  \u958B\u767A\u8005\u306F\u6587\u5B57\u5217\u306E\u691C\u7D22\u3001\u4E00\u81F4\u3001\
  \u305D\u3057\u3066\u9AD8\u5EA6\u306A\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\
  \u30B0\u6280\u8853\u3067\u306E\u64CD\u4F5C\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\
  \u3059\u3002Rust\u3067\u306Eregex\u306E\u5229\u7528\u306F\u3001\u30C6\u30AD\u30B9\
  \u30C8\u30C7\u30FC\u30BF\u306E\u89E3\u6790\u3068\u51E6\u7406\u3092\u52B9\u7387\u7684\
  \u306B\u884C\u3046\u306E\u306B\u5F79\u7ACB\u3061\u3001\u30C7\u30FC\u30BF\u691C\u8A3C\
  \u3001\u691C\u7D22\u3001\u30C6\u30AD\u30B9\u30C8\u5909\u63DB\u306A\u3069\u306E\u30BF\
  \u30B9\u30AF\u3092\u3088\u308A\u7C21\u6F54\u304B\u3064\u4FDD\u5B88\u3057\u3084\u3059\
  \u304F\u3057\u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.805664-06:00'
model: gpt-4-0125-preview
summary: "\u6B63\u898F\u8868\u73FE\uFF08regex\uFF09\u3092\u4F7F\u3046\u3068\u3001\u958B\
  \u767A\u8005\u306F\u6587\u5B57\u5217\u306E\u691C\u7D22\u3001\u4E00\u81F4\u3001\u305D\
  \u3057\u3066\u9AD8\u5EA6\u306A\u30D1\u30BF\u30FC\u30F3\u30DE\u30C3\u30C1\u30F3\u30B0\
  \u6280\u8853\u3067\u306E\u64CD\u4F5C\u304C\u53EF\u80FD\u306B\u306A\u308A\u307E\u3059\
  \u3002Rust\u3067\u306Eregex\u306E\u5229\u7528\u306F\u3001\u30C6\u30AD\u30B9\u30C8\
  \u30C7\u30FC\u30BF\u306E\u89E3\u6790\u3068\u51E6\u7406\u3092\u52B9\u7387\u7684\u306B\
  \u884C\u3046\u306E\u306B\u5F79\u7ACB\u3061\u3001\u30C7\u30FC\u30BF\u691C\u8A3C\u3001\
  \u691C\u7D22\u3001\u30C6\u30AD\u30B9\u30C8\u5909\u63DB\u306A\u3069\u306E\u30BF\u30B9\
  \u30AF\u3092\u3088\u308A\u7C21\u6F54\u304B\u3064\u4FDD\u5B88\u3057\u3084\u3059\u304F\
  \u3057\u307E\u3059\u3002"
title: "\u6B63\u898F\u8868\u73FE\u306E\u4F7F\u7528"
weight: 11
---

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
