---
aliases:
- /ja/rust/using-a-debugger/
date: 2024-01-26 04:10:23.722536-07:00
description: "\u30C7\u30D0\u30C3\u30AC\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u306F\
  \u3001\u30B3\u30FC\u30C9\u5B9F\u884C\u306E\u5185\u90E8\u3092X\u7DDA\u3067\u898B\u308B\
  \u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\
  \u30D0\u30B0\u3092\u7279\u5B9A\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u6D41\u308C\
  \u3092\u7406\u89E3\u3057\u3001\u30B3\u30FC\u30C9\u304C\u6975\u3081\u3066\u30AF\u30EA\
  \u30A2\u3067\u3042\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002\u307E\u308B\u3067\u3001\u3069\u3053\
  \u3067\u3064\u307E\u305A\u3044\u305F\u304B\u3092\u6B63\u78BA\u306B\u6307\u6458\u3057\
  \u3066\u304F\u308C\u308B\u53CB\u4EBA\u306E\u3088\u3046\u306A\u3082\u306E\u3067\u3059\
  \u3002"
lastmod: 2024-02-18 23:08:54.725559
model: gpt-4-0125-preview
summary: "\u30C7\u30D0\u30C3\u30AC\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u306F\
  \u3001\u30B3\u30FC\u30C9\u5B9F\u884C\u306E\u5185\u90E8\u3092X\u7DDA\u3067\u898B\u308B\
  \u3088\u3046\u306A\u3082\u306E\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u306F\
  \u30D0\u30B0\u3092\u7279\u5B9A\u3001\u30D7\u30ED\u30B0\u30E9\u30E0\u306E\u6D41\u308C\
  \u3092\u7406\u89E3\u3057\u3001\u30B3\u30FC\u30C9\u304C\u6975\u3081\u3066\u30AF\u30EA\
  \u30A2\u3067\u3042\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3059\u308B\u305F\u3081\u306B\
  \u3053\u308C\u3092\u884C\u3044\u307E\u3059\u3002\u307E\u308B\u3067\u3001\u3069\u3053\
  \u3067\u3064\u307E\u305A\u3044\u305F\u304B\u3092\u6B63\u78BA\u306B\u6307\u6458\u3057\
  \u3066\u304F\u308C\u308B\u53CB\u4EBA\u306E\u3088\u3046\u306A\u3082\u306E\u3067\u3059\
  \u3002"
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
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
