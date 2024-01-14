---
title:    "Rust: 部分文字列の抽出"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ

Rustでは、文字列から部分文字列を抽出する必要があるかもしれません。例えば、入力された文字列から特定の単語を検索したり、文字列の一部を取り出して表示したりする場合に便利です。この記事では、Rustで部分文字列を抽出する方法を紹介します。

## 方法

部分文字列を抽出するためには、文字列からの範囲を指定する必要があります。Rustでは、`[start..end]`という形式で範囲を指定することができます。例えば、`str[start..end]`とすることで、`start`から`end`までの文字列を抽出することができます。

以下のコードは、文中から特定の単語を抽出する例です。

```Rust
let text = "こんにちは、私はRustを勉強中です";
let keyword = "Rust";
let start = text.find(keyword).unwrap(); // 検索したキーワードの開始位置を取得
let end = start + keyword.len(); // キーワードの長さを加算
let result = &text[start..end];

println!("{}", result); // "Rust"が表示される
```

このコードでは、まず文字列から`Rust`という単語を検索し、その開始位置を`start`に保存します。その後、開始位置にキーワードの長さを加算して、`str[start..end]`で部分文字列を抽出します。

## 深堀り

部分文字列を抽出する際に気を付けるポイントとしては、範囲指定の`end`が実際の文字列の長さを超えないようにすることです。そうでないと、実行時エラーが発生してプログラムが停止します。

また、`[start..end]`の形式以外にも、`[start..]`という形式で"開始位置から最後まで"を表すこともできます。さらに、`[..end]`という形式で"最初から終了位置まで"を表すこともできます。

さらに、Rustの`str`型には便利なメソッドがいくつか用意されています。例えば、`contains()`を使うことで文字列が特定の文字列を含んでいるかどうかを確認できます。

## 関連リンク

- [The Rust Programming Language Documentation - Strings](https://doc.rust-lang.org/std/string/)
- [Rust by Example - Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Rust Stringのつかいかた](https://qiita.com/r9d1003w/items/d7af39b51fe8574c4469)
- [String型のメソッドまとめ](https://qiita.com/magicant/items/ca956fbba2364e67e40c)