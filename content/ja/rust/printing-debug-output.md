---
title:                "デバッグ出力の印刷"
html_title:           "Fish Shell: デバッグ出力の印刷"
simple_title:         "デバッグ出力の印刷"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## 何となぜ？

デバッグ出力の印刷とは、あなたのコードがどのように動作しているか、またはそれがうまく行かない理由を理解するための方法です。それはエラーやバグを特定し、解決する最善の道です。

## 実装方法：

Rustでは、デバッグ出力は `println!`マクロを使用して印刷できます。

```Rust 
fn main() {
    let name = "Rust";
    println!("Hello, {}!", name); 
}
```

出力:

```Shell
Hello, Rust!
```

同様に、デバッグ専用の `println!` バージョンが `dbg!` マクロです。

```Rust 
fn main() {
    let name = "Rust";
    dbg!(name);
}
```

出力:

```Shell
[src/main.rs:3] name = "Rust"
```

## ディープダイブ：

デバッグ出力の印刷は古くから存在し、疑わしいコードの動作を理解するための最も素朴な方法の1つです。多くの他の言語では、いわゆる「printfデバッグ」が行われてきました。

Rustでは上記以外にもデバッグマクロがいくつかあります。たとえば、 `eprintln!`マクロはエラーメッセージを標準エラー出力に直接書き出すことができます。

デバッグマクロの実装についても少し触れてみましょう。これらのマクロは、コンパイル時に実際の出力コードに置き換えられます。そのため、実行時のパフォーマンスに影響を与えません。

## 参考文献：

- Rust公式ドキュメンテーション: https://doc.rust-lang.org/stable/book
- Rustプリントと書式設定：https://doc.rust-lang.org/stable/rust-by-example/hello/print.html
- Rustによるデバッグ：https://doc.rust-lang.org/rust-by-example/hello/print.html