---
title:                "テキストの検索と置換"
date:                  2024-01-20T17:58:36.056769-07:00
model:                 gpt-4-1106-preview
simple_title:         "テキストの検索と置換"

tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/searching-and-replacing-text.md"
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
