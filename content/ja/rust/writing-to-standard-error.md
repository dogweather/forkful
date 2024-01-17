---
title:                "「標準エラーへの書き込み」"
html_title:           "Rust: 「標準エラーへの書き込み」"
simple_title:         "「標準エラーへの書き込み」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## ナニ & ナゼ？

標準エラーへの書き込みとは、プログラマーがエラー情報を出力するための方法です。エラーが発生した場合、プログラムは通常、標準エラーにメッセージを書き込みます。これにより、ユーザーはエラーが発生したことを知ることができ、必要な措置を講じることができます。

## 方法：

標準エラーに書き込むには、Rustの標準ライブラリにある`eprint!`または`eprintln!`マクロを使用します。これらのマクロは、コンソールにメッセージを出力します。以下は、その使用例です。

```Rust
fn main() {
    eprintln!("エラーが発生しました。");
}
```

出力は以下のようになります。

```
エラーが発生しました。
```

## 詳しく見る：

標準エラーへの書き込みは、プログラミングの世界で一般的な方法です。この仕組みは、コンピューターの初期の日々に遡ることができます。また、`eprint!`と`eprintln!`以外にも、標準エラーへの書き込みを行うための他の方法があります。しかし、Rustの場合は、これらのマクロが最も一般的な方法です。

## 関連情報：

- Rustの公式ドキュメント: https://doc.rust-lang.org/std/macro.eprintln.html
- 標準エラーの仕組みについての詳細: https://en.wikipedia.org/wiki/Standard_error_(computing)