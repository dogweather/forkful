---
title:                "デバッグ出力のプリント"
html_title:           "Rust: デバッグ出力のプリント"
simple_title:         "デバッグ出力のプリント"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## なぜ
  デバッグ出力を表示することのメリットを最大2文で説明します。

デバッグ出力を使用すると、コードのエラーを特定し、修正することができます。また、コードの実行を追跡し、理解するのに役立ちます。

## 方法
```Rust
fn main() {
    let num = 5;
    println!("The value of num is {}", num); // numの値を出力
}
```

上記の`println!`マクロを使用して、変数の値を出力することができます。`{}`内に変数名を記述することで、その値を表示することができます。このような方法で、複数の変数の値を同時に表示することもできます。

## ディープダイブ
デバッグ出力をより詳しく見てみましょう。`println!`マクロの他にも、`eprintln!`や`dbg!`というマクロもあります。`eprintln!`は、エラー出力のみを表示する際に使用します。`dbg!`は、デバッグ出力用のマクロで、`println!`と同様に変数の値を表示することができますが、関数や構造体などの情報も表示することができます。

## See Also
- Rustプログラミング言語公式サイト: https://www.rust-lang.org/ja/
- Rustチュートリアル: https://doc.rust-jp.rs/the-rust-programming-language-ja/1.10/book/getting-started.html
- Rustコミュニティフォーラム: https://users.rust-lang.org/

この記事ではデバッグ出力について説明しましたが、Rustには他にも多くの便利な機能があります。ぜひ公式サイトやチュートリアル、コミュニティフォーラムなどでさらに学んでみてください。