---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:38.281328-07:00
description: "Rust\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\
  \u3068\u306F\u3001\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u304C\u6587\
  \u5B57\u3067\u3042\u308B\u5834\u5408\u306B\u5927\u6587\u5B57\u306B\u5909\u66F4\u3057\
  \u3001\u6B8B\u308A\u306E\u6587\u5B57\u5217\u3092\u5909\u66F4\u3057\u306A\u3044\u3088\
  \u3046\u306B\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u30FC\u306F\u3001\u30BF\u30A4\u30C8\u30EB\u306E\u305F\u3081\u306E\
  \u5358\u8A9E\u306E\u6E96\u5099\u3084\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u4E00\
  \u8CAB\u6027\u3092\u78BA\u4FDD\u3059\u308B\u306A\u3069\u3001\u30D5\u30A9\u30FC\u30DE\
  \u30C3\u30C8\u76EE\u7684\u3067\u3053\u306E\u64CD\u4F5C\u3092\u983B\u7E41\u306B\u884C\
  \u3044\u307E\u3059\u3002"
lastmod: 2024-02-19 22:05:00.975061
model: gpt-4-0125-preview
summary: "Rust\u3067\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u5316\u3059\u308B\u3068\
  \u306F\u3001\u6587\u5B57\u5217\u306E\u6700\u521D\u306E\u6587\u5B57\u304C\u6587\u5B57\
  \u3067\u3042\u308B\u5834\u5408\u306B\u5927\u6587\u5B57\u306B\u5909\u66F4\u3057\u3001\
  \u6B8B\u308A\u306E\u6587\u5B57\u5217\u3092\u5909\u66F4\u3057\u306A\u3044\u3088\u3046\
  \u306B\u3059\u308B\u3053\u3068\u3092\u6307\u3057\u307E\u3059\u3002\u30D7\u30ED\u30B0\
  \u30E9\u30DE\u30FC\u306F\u3001\u30BF\u30A4\u30C8\u30EB\u306E\u305F\u3081\u306E\u5358\
  \u8A9E\u306E\u6E96\u5099\u3084\u30E6\u30FC\u30B6\u30FC\u5165\u529B\u306E\u4E00\u8CAB\
  \u6027\u3092\u78BA\u4FDD\u3059\u308B\u306A\u3069\u3001\u30D5\u30A9\u30FC\u30DE\u30C3\
  \u30C8\u76EE\u7684\u3067\u3053\u306E\u64CD\u4F5C\u3092\u983B\u7E41\u306B\u884C\u3044\
  \u307E\u3059\u3002"
title: "\u6587\u5B57\u5217\u3092\u5927\u6587\u5B57\u306B\u3059\u308B"
---

{{< edit_this_page >}}

## 何となぜ？

Rustで文字列を大文字化するとは、文字列の最初の文字が文字である場合に大文字に変更し、残りの文字列を変更しないようにすることを指します。プログラマーは、タイトルのための単語の準備やユーザー入力の一貫性を確保するなど、フォーマット目的でこの操作を頻繁に行います。

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
