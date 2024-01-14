---
title:                "Rust: 文字列の抽出"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ
文字列の一部を抽出することは、プログラミングでよく使われる機能です。Rustでは、文字列の一部を抽出することを非常に効率的に行うことができます。この記事では、Rustで文字列の一部を抽出する方法を学びましょう。

## 方法
まず、Rustの標準ライブラリである`str`クラスから、`slice()`メソッドを使用して部分文字列を抽出することができます。以下の例をご覧ください。

```Rust
let my_string = "Hello world!";
let substring = &my_string[0..5];
println!("{}", substring);
```

上記のコードでは、`my_string`から最初の5文字を抽出し、`substring`に代入しています。そして、`println!`マクロを使用して、抽出した部分文字列を出力しています。実行結果は次の通りです。

```
Hello
```

また、Rustでは文字列スライスという機能も利用することができます。これを使うと、もとの文字列の一部を変更することなく、部分文字列を取得することができます。

```Rust
let my_string = String::from("Rust is awesome!");
let substring = &my_string[0..4];
println!("{}", my_string);
```

上記のコードでは、`my_string`の最初の4文字を抽出し、`substring`に代入しています。しかし、実際に`my_string`を出力すると、もとの文字列が変更されず、`"Rust is awesome!"`と出力されます。

## ディープダイブ
文字列の一部を抽出する方法はいくつかありますが、Rustではパフォーマンスの観点から`str`クラスのメソッドを使用することが推奨されています。また、文字列スライスを使用するときには、`String`型ではなく`&str`型を使用することで、パフォーマンスの向上が期待できます。さらに、文字列スライスを使用する場合は、インデックス数や範囲に注意する必要があります。

＃＃ おすすめのリンク
Rustでは文字列の一部を抽出する方法が様々ありますが、今回紹介した方法が最も実用的だと考えられます。さらに詳しい情報が必要な場合は、以下のリンクを参考にすることをおすすめします。

- [Rustの公式ドキュメント](https://doc.rust-lang.org/std/primitive.str.html#method.slice)
- [Rust By Example - Slicing](https://doc.rust-lang.org/rust-by-example/std/str/slicing.html)
- [はじめてのRust - 文字列スライス](https://tech-blog.sgr-ksmt.org/2014/09/14/rust_starting_ep3/)
- [もう迷わない、Rustの文字列操作入門](https://qiita.com/kotlin/items/808c06fe550a2f2d9719)