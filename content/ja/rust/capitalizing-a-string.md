---
title:                "文字列を大文字にする"
aliases:
- ja/rust/capitalizing-a-string.md
date:                  2024-02-03T19:06:38.281328-07:00
model:                 gpt-4-0125-preview
simple_title:         "文字列を大文字にする"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
