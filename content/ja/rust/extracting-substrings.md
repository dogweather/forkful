---
title:                "Rust: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## なぜ

文字列から部分文字列を抽出する方法を学ぶのは、Rustプログラミングの中で非常に役立ちます。例えば、入力された文字列の中から特定の情報を取得したり、文字列を検索してデータを整理したりする場合に、部分文字列を抽出する必要があります。また、文字列の処理は多くのアプリケーションで重要な役割を果たすため、部分文字列を効率的に取得することはプログラミングのスキルとして非常に重要です。

## 使い方

部分文字列を抽出するには、Rustでは`&str`型のメソッドを使用します。このメソッドを使用すると、文字列内の任意の位置から特定の文字数だけを取り出すことができます。例えば、次のコードは`input`という文字列から`ello`という部分文字列を抽出し、出力します。

```Rust
let input = "Hello, world!";
let output = &input[1..5];
println!("{}", output); // 出力結果: ello
```

また、特定の文字列を検索して部分文字列を抽出することも可能です。次のコードは`input`という文字列から`world`という単語を検索し、見つけた場合はその部分文字列を出力します。

```Rust
let input = "Hello, world!";
if let Some(index) = input.find("world") {
  let output = &input[index..index+5];
  println!("{}", output); // 出力結果: world
}
```

## ディープダイブ

部分文字列を抽出する方法についてもっと詳しく知りたい場合は、Rustの公式ドキュメントを参考にしてください。そこでは`&str`型や`String`型に対して利用可能なさまざまなメソッドが詳しく説明されています。また、正規表現を使って部分文字列を抽出する方法など、さまざまなテクニックも学べます。

## 参考リンク

- [Rustの公式ドキュメント](https://doc.rust-lang.org/std/primitive.str.html#method.slice)
- [部分文字列を抽出するための正規表現の書き方](https://docs.rs/regex/1.5.4/regex/#what-are-regular-expressions)