---
title:                "「文字列の大文字化」"
html_title:           "Rust: 「文字列の大文字化」"
simple_title:         "「文字列の大文字化」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列を大文字に変換することの網じめについて説明するために、なぜこれを行うのかという点を最初に説明したいと思います。文字列の大文字変換は、ユーザーの入力を正規化したり、表示の一貫性を保ったりするために使用される一般的なタスクです。

## 方法

Rustでは、文字列を大文字に変換するための便利なメソッドが用意されています。下記のコードを使用することで、文字列"hello"を"HELLO"に変換することができます。

```Rust
let small_str = "hello";
let capital_str = small_str.to_uppercase();
```

コードの実行結果は、`capital_str`に"HELLO"という値が格納されることになります。このメソッドは、ASCII文字だけでなくUnicode文字にも対応しています。

## ディープダイブ

Rustにおける文字列の大文字変換には、内部的にバイトシーケンスが使用されています。このバイトシーケンスは、コンピューター内部で文字を表現する際に使用される方法です。Rustは文字列がUTF-8でエンコードされていることを保証するため、このバイトシーケンスを使用して処理を行います。そのため、文字列の大文字変換を行う際にもUTF-8のルールに従って処理が行われます。

## 参考リンク

ここまで文字列の大文字変換について紹介してきましたが、Rustにはさまざまな便利な文字列操作のメソッドが用意されています。興味がある方は、下記のリンクを参考にしてみてください。

- [Rust 文字列処理 - Rust by Example (日本語訳)](https://doc.rust-jp.rs/rust-by-example-ja/std/str/process.html)
- [日本語で学ぶRustチュートリアル - 03. 文字列 -](https://doc.rust-jp.rs/the-rust-programming-language-ja/1.6/book/strings.html)
- [Rust の文字列とUTF-8 - Qiita](https://qiita.com/satosystems/items/bf42219680401fb32067)