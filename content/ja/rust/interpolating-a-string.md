---
title:                "文字列の補間"
html_title:           "Rust: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

## なに＆なぜ？
文字列内の変数や式を埋め込むことを文字列補間と呼びます。プログラマーは文字列補間を使用することで、動的なメッセージやデータを簡単に生成することができます。

## 方法：
Rustでは、バッククォート記号 ``` で囲まれた文字列に、{}で囲まれた変数や式を埋め込むことができます。以下の例を参考にしてください。

```Rust
let age = 25;
let message = `I am {} years old.`;

println!(message, age);

// 出力結果：I am 25 years old.
```

さらに、{}内にformat!マクロを使用することで、より複雑なメッセージを作成することもできます。

```Rust
let name = "John";
let age = 25;

let message = format!(`My name is {} and I am {} years old.`, name, age);

println!(message);

// 出力結果：My name is John and I am 25 years old.
```

## 詳細：
文字列補間は、Rustや他のプログラミング言語でも一般的に使用されています。C言語では、%記号を使用したフォーマット文字列を使用して、変数や式を文字列に埋め込むことができましたが、Rustでは{}を使用することでより柔軟に利用することができます。

文字列補間の代わりに、文字列結合を使用することもできますが、煩雑なコードになりやすいため、文字列補間が好まれます。また、バッククォートではなく、ダブルクォートを使用した場合は、変数の型によってはコンパイルエラーが発生することがあるので注意が必要です。

## 関連リンク：
- [Rustプログラミング言語公式サイト](https://www.rust-lang.org/ja/)
- [Rustの文字列補間のドキュメント](https://doc.rust-lang.org/std/fmt/#formatting-strings)