---
date: 2024-01-20 17:58:36.056769-07:00
description: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u6587\
  \u5B57\u5217\u306E\u4E2D\u304B\u3089\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u3092\
  \u898B\u3064\u3051\u51FA\u3057\u3001\u305D\u308C\u3092\u5225\u306E\u30C6\u30AD\u30B9\
  \u30C8\u3067\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u306F\u30C7\u30FC\u30BF\u3092\u6574\u7406\u3057\u305F\u308A\u3001\
  \u30B3\u30FC\u30C9\u3092\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3057\u305F\
  \u308A\u3059\u308B\u6642\u306B\u3053\u308C\u3092\u3088\u304F\u884C\u3044\u307E\u3059\
  \u3002"
isCJKLanguage: true
lastmod: '2024-02-25T18:49:39.855693-07:00'
model: gpt-4-1106-preview
summary: "\u30C6\u30AD\u30B9\u30C8\u691C\u7D22\u3068\u7F6E\u63DB\u306F\u3001\u6587\
  \u5B57\u5217\u306E\u4E2D\u304B\u3089\u7279\u5B9A\u306E\u30D1\u30BF\u30FC\u30F3\u3092\
  \u898B\u3064\u3051\u51FA\u3057\u3001\u305D\u308C\u3092\u5225\u306E\u30C6\u30AD\u30B9\
  \u30C8\u3067\u7F6E\u304D\u63DB\u3048\u308B\u3053\u3068\u3067\u3059\u3002\u30D7\u30ED\
  \u30B0\u30E9\u30DE\u306F\u30C7\u30FC\u30BF\u3092\u6574\u7406\u3057\u305F\u308A\u3001\
  \u30B3\u30FC\u30C9\u3092\u30EA\u30D5\u30A1\u30AF\u30BF\u30EA\u30F3\u30B0\u3057\u305F\
  \u308A\u3059\u308B\u6642\u306B\u3053\u308C\u3092\u3088\u304F\u884C\u3044\u307E\u3059\
  \u3002"
title: "\u30C6\u30AD\u30B9\u30C8\u306E\u691C\u7D22\u3068\u7F6E\u63DB"
---

{{< edit_this_page >}}

## What & Why? / 何となぜ？

テキスト検索と置換は、文字列の中から特定のパターンを見つけ出し、それを別のテキストで置き換えることです。プログラマはデータを整理したり、コードをリファクタリングしたりする時にこれをよく行います。

## How to: / 方法：

```Rust
fn main() {
    let text = "Hello, World!";
    let search = "World";
    let replace_with = "Rust";

    let replaced_text = text.replace(search, replace_with);

    println!("{}", replaced_text); // "Hello, Rust!"
}
```

このコードは、"World"を"Rust"に置き換えています。

## Deep Dive / 詳細情報：

文字列の検索と置換はコンピューティングの初期からある基本的な操作です。Rustでは`.replace()`関数がこれを簡単に行います。代替手段として正規表現を使うことがありますが、`regex`クレートが必要です。正確さと柔軟性を求める場合は、正規表現による検索と置換がよく利用されます。Rustでの文字列操作は、所有権と借用の規則に注意しながら行う必要がある点も特徴的です。

## See Also / 関連情報：

- Rustの公式ドキュメントにある`.replace()`メソッド: https://doc.rust-lang.org/std/string/struct.String.html#method.replace
- `regex`クレートのドキュメント: https://crates.io/crates/regex
- 文字列操作に関するRust by Example: https://doc.rust-lang.org/rust-by-example/std/str.html
