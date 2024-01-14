---
title:                "Rust: 「デバッグ出力のプリント」"
simple_title:         "「デバッグ出力のプリント」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ

デバッグ出力をすることのメリットを知っていますか？デバッグ出力は、プログラミングのプロセスで非常に重要な役割を果たします。特にRustのような静的型付け言語では、デバッグ出力はコードのバグを特定するのに非常に役立ちます。デバッグ出力を行うことで、プログラムのどの部分で問題が発生しているかを特定し、修正することができます。

## 方法

デバッグ出力を行うには、Rustの組み込みマクロである`println！`を使用します。例えば、`println！（"Hello world！"）;`というコードを実行すると、`Hello world！`がコンソールに出力されます。変数を出力するには、`println！（"変数xの値は{}です。"、x）;`のようにコードを記述します。また、複数の変数を出力するには、`println！（"変数xの値は{}であり、変数yの値は{}です。"、x、y）;`のようにコードを書きます。

以下に、デバッグ出力の例を示します。

```Rust
fn main(){
    let name = "John";
    let age = 25;
    println!("My name is {} and I am {} years old.", name, age);
}
```

出力結果：
```
My name is John and I am 25 years old.
```

## ディープダイブ

`println！`マクロは、文字列や変数の値を出力するだけではありません。さまざまな形式のデータを出力することができます。例えば、`{:?}`フォーマット指定子を使用すると、デバッグ用の出力を行うことができます。また、`{:#?}`を使えば、より見やすい形式で出力することができます。さらに、カスタムのデバッグ用出力を行うために`Debug`トレイトを実装することもできます。

## See Also
- [Rust公式ドキュメント](https://doc.rust-lang.org/std/macro.println.html)
- [Rustプログラミング入門](https://rust-lang-ja.github.io/the-rust-programming-language-ja/1.6/book/index.html)