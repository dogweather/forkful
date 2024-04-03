---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:38.281328-07:00
description: "\u3069\u3046\u3084\u3063\u3066\uFF1A Rust\u3067\u6587\u5B57\u5217\u3092\
  \u5927\u6587\u5B57\u5316\u3059\u308B\u306B\u306F\u3001\u4E3B\u306B2\u3064\u306E\u65B9\
  \u6CD5\u304C\u3042\u308A\u307E\u3059\uFF1A\u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\
  \u306E\u6A5F\u80FD\u3092\u4F7F\u7528\u3059\u308B\u304B\u3001\u3088\u308A\u8907\u96D1\
  \u307E\u305F\u306F\u7279\u5B9A\u306E\u30CB\u30FC\u30BA\u306B\u5BFE\u5FDC\u3059\u308B\
  \u305F\u3081\u306B\u30B5\u30FC\u30C9\u30D1\u30FC\u30C6\u30A3\u306E\u30AF\u30EC\u30FC\
  \u30C8\u3092\u5229\u7528\u3057\u307E\u3059\u3002\u3069\u3061\u3089\u3082\u4EE5\u4E0B\
  \u306E\u65B9\u6CD5\u3067\u53EF\u80FD\u3067\u3059\u3002 #."
lastmod: '2024-03-13T22:44:41.794326-06:00'
model: gpt-4-0125-preview
summary: "Rust\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u306B\
  \u306F\u3001\u4E3B\u306B2\u3064\u306E\u65B9\u6CD5\u304C\u3042\u308A\u307E\u3059\uFF1A\
  \u6A19\u6E96\u30E9\u30A4\u30D6\u30E9\u30EA\u306E\u6A5F\u80FD\u3092\u4F7F\u7528\u3059\
  \u308B\u304B\u3001\u3088\u308A\u8907\u96D1\u307E\u305F\u306F\u7279\u5B9A\u306E\u30CB\
  \u30FC\u30BA\u306B\u5BFE\u5FDC\u3059\u308B\u305F\u3081\u306B\u30B5\u30FC\u30C9\u30D1\
  \u30FC\u30C6\u30A3\u306E\u30AF\u30EC\u30FC\u30C8\u3092\u5229\u7528\u3057\u307E\u3059\
  \u3002\u3069\u3061\u3089\u3082\u4EE5\u4E0B\u306E\u65B9\u6CD5\u3067\u53EF\u80FD\u3067\
  \u3059."
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
weight: 2
---

## どうやって：
Rustで文字列を大文字化するには、主に2つの方法があります：標準ライブラリの機能を使用するか、より複雑または特定のニーズに対応するためにサードパーティのクレートを利用します。どちらも以下の方法で可能です。

### Rustの標準ライブラリを使用する
Rustの標準ライブラリには文字列を大文字化する直接的な方法がありませんが、文字列の文字を操作することでこれを達成できます。

```rust
fn capitalize_first(s: &str) -> String {
    let mut c = s.chars();
    match c.next() {
        None => String::new(),
        Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
    }
}

fn main() {
    let my_string = "hello";
    println!("{}", capitalize_first(my_string)); // 出力: Hello
}
```

### `heck` クレートを使用する
より大規模なテキスト処理コンテキスト内で作業する場合、特に直接的なアプローチを好むなら、`heck` などのサードパーティライブラリの使用を好むかもしれません。`heck` クレートは、文字列を大文字化する簡単な方法を含む、さまざまなケース変換機能を提供します。

まず、`Cargo.toml`に`heck`を追加します：

```toml
[dependencies]
heck = "0.4.0"
```

次に、文字列を大文字化するためにそれを使用します：

```rust
extern crate heck; // Rust 2018 edition以降では不要
use heck::TitleCase;

fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // 出力: Hello World
}
```

注意：`heck`によって提供される`to_title_case`メソッドは、文字列内の各単語を大文字化しますが、文字列の最初の文字のみを大文字化したい場合は、求めているもの以上かもしれません。特定のニーズに応じて使用方法を調整してください。
