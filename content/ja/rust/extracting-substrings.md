---
title:                "サブストリングの抽出"
html_title:           "Rust: サブストリングの抽出"
simple_title:         "サブストリングの抽出"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
サブストリングを抽出するとは何か、そしてなぜプログラマーたちはそれをするのかを説明します。

## 方法：
```Rust
let string = "Hello, world!";
let substring = &string[0..5];
```

コンソール出力:`Hello`

サブストリングを作成するには、元の文字列から必要な部分を指定することで行うことができます。コードブロック内の実例を参照してください。

## 詳細を調べる：
サブストリングの抽出は、文字列を効率的に操作する上で重要な役割を担ってきました。過去のプログラミング言語では、サブストリングの抽出に制限がありましたが、Rustでは完全にサポートされています。他の選択肢としては、文字列の操作には正規表現を使用する方法がありますが、これはRustのような新しい言語ではあまり一般的ではありません。サブストリングの抽出は、文字列の一部を簡単に取り出すことができる便利な方法です。

## 関連情報：
- [Rustのドキュメンテーション](https://doc.rust-lang.org/std/primitive.str.html#method.slice)
- [正規表現についてのチュートリアル](https://www.regular-expressions.info/tutorial.html)