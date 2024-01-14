---
title:                "Rust: 文字列の連結"
simple_title:         "文字列の連結"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/concatenating-strings.md"
---

{{< edit_this_page >}}

## なぜ

文字列の連結に取り組む理由は、より効率的に文字列を処理するためです。Rustの文字列結合の方法を学ぶことで、コードのパフォーマンスを向上させることができます。

## 方法

```Rust
let name = "山田";
let age = 25;
let greeting = format!("こんにちは、{}さん。あなたは{}歳です。", name, age);
println!("{}", greeting);
```

この例では、`format!()`マクロを使用して文字列を連結し、`println!()`マクロを使用して文字列を出力しています。`format!()`を使用することで、複数の変数を含む文字列を作成できます。`println!()`を使用することで、作成した文字列をターミナルに表示することができます。

出力結果:

```
こんにちは、山田さん。あなたは25歳です。
```

## ディープダイブ

`format!()`マクロは、文字列の連結に必要なメモリーの割り当てを最小限に抑えることができます。これは、文字列の連結が深いネストの場合でも同様です。また、`format!()`を使用することで、文字列のスライスを結合するよりも高速に処理することができます。

また、Rustでは`String`と`&str`の間での変換が自動的に行われるため、`String`を作成してから、さらに文字列を結合することもできます。

## See Also
- [Rustの公式ドキュメント - String型](https://doc.rust-lang.org/std/string/index.html)
- [Efficient String Concatenation in Rust](https://endler.dev/2017/string-concatenation-in-rust/)
- [Rustで文字列の結合をする方法について見てみる](https://qiita.com/7shi/items/952d692e824d51eecfe1)