---
title:                "「文字列の連結」"
html_title:           "Rust: 「文字列の連結」"
simple_title:         "「文字列の連結」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

こんにちは、みなさん！今日はRustの最新バージョンについてお話ししましょう。今回のテーマは「文字列の連結」です。Rustには便利な文字列の操作方法がたくさんありますが、なぜ私たちは文字列を連結するのでしょうか？それは、複数の文字列を一つにまとめることで、より複雑なデータやメッセージを処理するための基本的な手段だからです。ぜひ、今回の記事でRustの文字列の連結方法を学んでみてください。

## どのように

まずは、基本的な文字列の連結方法を見ていきましょう。Rustでは、`+`演算子を使って複数の文字列を連結することができます。例えば、以下のようなコードを書いてみましょう。

```rust
let greeting = "こんにちは、";
let name = "みなさん！";
let message = greeting + name;

println!("{}", message);
```

このコードでは、`+`演算子を使って`greeting`と`name`を連結し、`message`に代入しています。そして、`println!`マクロを使って`message`を出力しています。実行すると、`こんにちは、みなさん！`というメッセージが出力されるはずです。

もう少し複雑な例を見てみましょう。例えば、数字を文字列に変換して他の文字列と連結することもできます。

```rust
let number = 123;
let message = "今日は" + number.to_string() + "日目です。";

println!("{}", message);
```

この場合は、まず`number`を`to_string()`メソッドを使って文字列に変換し、その後に`+`演算子を使って他の文字列と連結しています。実行すると、`今日は123日目です。`というメッセージが出力されるはずです。

## ディープダイブ

Rustでは、文字列を連結するための便利なメソッドが用意されています。例えば、`push_str()`メソッドを使うと、既存の文字列に別の文字列を追加することができます。

```rust
let mut message = String::from("おはよう");

message.push_str("ございます。");

println!("{}", message);
```

また、`format!`マクロを使うことで、複数の文字列や変数を簡単に連結することができます。

```rust
let name = "太郎";
let age = 20;

let message = format!("私の名前は{}で、年齢は{}歳です。", name, age);

println!("{}", message);
```

このように、Rustでは様々な方法で文字列を連結することができます。なお、文字列の連結には`+`演算子を使うよりも`format!`マクロを使った方がパフォーマンス面で優れているため、文字列を連結する場合はできる限り`format!`マクロを使うことをおすすめします。

## See Also

もしRustの文字列操作についてもっと知りたい場合は、以下のリンクを参考にしてみてください。

- [Rust公式ドキュメント](https://doc.rust-lang.org/stable/std/string/)
- [Rust by Example - Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [The Rust Programming Language Book - Strings](https