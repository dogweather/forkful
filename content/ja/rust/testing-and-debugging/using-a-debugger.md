---
title:                "デバッガーの使い方"
aliases:
- /ja/rust/using-a-debugger.md
date:                  2024-01-26T04:10:23.722536-07:00
model:                 gpt-4-0125-preview
simple_title:         "デバッガーの使い方"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/using-a-debugger.md"
---

{{< edit_this_page >}}

## 何となぜ？

デバッガを使用することは、コード実行の内部をX線で見るようなものです。プログラマはバグを特定、プログラムの流れを理解し、コードが極めてクリアであることを確認するためにこれを行います。まるで、どこでつまずいたかを正確に指摘してくれる友人のようなものです。

## 使い方:

Rustは様々なデバッガをサポートしていますが、GNU/Linuxでは `gdb`、macOSでは `lldb` が一般的です。`rust-gdb` や `rust-lldb` を使用することもできます。これらはRustの値をきれいに表示するラッパーです。一例を見てみましょう:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Counter is at: {}", counter);
    }
}
```

これをデバッグするには、デバッグ情報を含めてコンパイルします:

```shell
$ rustc -g counter.rs
```

その後、`rust-gdb` で実行します:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Counter is at: 1
(gdb) print counter
$2 = 1
```

## 詳細な解説

デバッグはパンチカードの時代からある古い技術で、その進化は神の恵みです。Rustは、システムレベルの特性を持つ言語であるため、GDBやLLDBとの統合を備えた独自のツールを提供しています。

Rustコードをデバッグする別の方法には、統合開発環境（IDE）を使用し、内蔵されているデバッガを活用する方法があり、これがより直感的であると感じる人もいます。人気のあるIDEには、Rustプラグイン付きのCLionや、Rust拡張機能付きのVisual Studio Codeがあります。

実装に関しては、Rustはこれらのデバッガが理解できるデバッグシンボルを生成します。これは、コードをステップ実行し、ブレークポイントを設定し、変数を検査する際に非常に重要です。

## 参照

- デバッグに関するRustのドキュメント: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- Rust By Exampleによるエラーとデバッグの解説: https://doc.rust-lang.org/rust-by-example/error.html
- VS CodeのRust拡張機能を動かすRust言語サーバー (RLS): https://github.com/rust-lang/rls
- Visual Studio CodeでのRustデバッグ: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
