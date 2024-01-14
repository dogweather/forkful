---
title:    "Rust: 文字列の長さを見つける"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ

文字列の長さを求めることについて説明します。Rust言語を使用することで、より効率的で安全な方法で文字列の長さを取得することができます。

## 方法

まず、以下のように `str::len()` 関数を使用して文字列の長さを取得することができます。

```Rust
let str = "こんにちは、世界！";
let length = str.len();
println!("文字列の長さは{}です。", length);

// 出力: 文字列の長さは9です。
```

また、バイト単位で長さを取得する場合は `str::as_bytes()` を使用することもできます。

```Rust
let str = "こんにちは、世界！";
let bytes = str.as_bytes();
println!("文字列の長さは{}バイトです。", bytes.len());

// 出力: 文字列の長さは24バイトです。
```

さらに、`Utf8Iterator`を使用することで、マルチバイト文字を正しく処理しながら文字列の長さを取得することができます。しかし、これはより高度なトピックであり、詳細な説明は省きます。

## ディープダイブ

Rustでは、文字列はUTF-8でエンコードされています。そのため、バイト数を文字列の長さとして単純に扱うことはできません。Rustの `str`型は、これらのマルチバイト文字を適切に処理するために、`Solidity` と呼ばれる特別なデータ型を使用しています。また、文字列の長さを取得する方法には、`bytes` メソッドや`chars` メソッドなどもありますが、それぞれのメソッドの違いや使用方法については、公式ドキュメントを参照することをお勧めします。 

## その他の参考

- [The Rust Programming Language](https://www.rust-lang.org/)
- [Rustのドキュメント](https://doc.rust-lang.org/)
- [Rustプログラミングの基本](https://www.oreilly.co.jp/books/9784873116002/)
- [Effective Rust](https://rust-lang.github.io/edition-guide/rust-2018/index.html)