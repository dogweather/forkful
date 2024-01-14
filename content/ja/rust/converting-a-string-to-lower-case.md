---
title:    "Rust: 文字列を小文字に変換する"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## なぜ
文字列を小文字に変換することの意義を説明します。Rust言語を学び始める人や、より良いコードを書くためのヒントをお探しの方にとって、理解することが重要です。

## 方法
まずは、標準ライブラリの `to_lowercase()` 関数を使ってみましょう。例えば、`Hello Rust`を `hello rust` に変換したい場合、以下のようなコードを書くことができます。

```Rust
let s = "Hello Rust".to_lowercase();
println!("{}", s);
```

出力結果は、`hello rust`となります。

また、自分で実装する方法もあります。以下のサンプルコードをご覧ください。

```Rust
fn convert_to_lowercase(s: &str) -> String {
    let mut output = String::new();
    for c in s.chars() {
        if c.is_uppercase() {
            output.push((c as u8 + 32) as char);
        } else {
            output.push(c);
        }
    }
    output
}

let s = "Hello Rust";
let lowercased = convert_to_lowercase(s);
println!("{}", lowercased);
```

このコードでは、文字列の各文字をチェックし、大文字ならばアスキーコードを32減算して小文字に変換しています。出力結果は同じく`hello rust`となります。

## ディープダイブ
文字列を小文字に変換する際、Unicode文字の処理や、他の言語との違いについて深く掘り下げていきましょう。Rustの `to_lowercase()` 関数は、UnicodeのNFC形式での変換を行います。また、UTF-8以外のエンコーディングでは、変換結果が異なることにも注意が必要です。

## 参考リンク
- [Rust標準ライブラリ: str::to_lowercase()](https://doc.rust-lang.org/std/primitive.str.html#method.to_lowercase)
- [Unicode Normalization Forms](https://unicode.org/faq/normalization.html)
- [UTF-8とは？](https://ja.wikipedia.org/wiki/UTF-8)

## もっと詳しく知りたい方へ
Rustの文字列処理については、[The Rust Programming Language](https://doc.rust-lang.org/book/)を参考にしてください。また、プログラミング言語全般の知識を深めたい方は、[プログラム言語の基礎](https://www.amazon.co.jp/プログラム言語の基礎-ヨハネス・グレーチェンバッハ/dp/4320123369)という本がおすすめです。