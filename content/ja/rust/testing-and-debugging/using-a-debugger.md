---
date: 2024-01-26 04:10:23.722536-07:00
description: "Rust\u306F\u69D8\u3005\u306A\u30C7\u30D0\u30C3\u30AC\u3092\u30B5\u30DD\
  \u30FC\u30C8\u3057\u3066\u3044\u307E\u3059\u304C\u3001GNU/Linux\u3067\u306F `gdb`\u3001\
  macOS\u3067\u306F `lldb` \u304C\u4E00\u822C\u7684\u3067\u3059\u3002`rust-gdb` \u3084\
  \ `rust-lldb` \u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3082\u3067\u304D\u307E\
  \u3059\u3002\u3053\u308C\u3089\u306FRust\u306E\u5024\u3092\u304D\u308C\u3044\u306B\
  \u8868\u793A\u3059\u308B\u30E9\u30C3\u30D1\u30FC\u3067\u3059\u3002\u4E00\u4F8B\u3092\
  \u898B\u3066\u307F\u307E\u3057\u3087\u3046: ```Rust fn\u2026"
lastmod: '2024-03-13T22:44:41.829858-06:00'
model: gpt-4-0125-preview
summary: "Rust\u306F\u69D8\u3005\u306A\u30C7\u30D0\u30C3\u30AC\u3092\u30B5\u30DD\u30FC\
  \u30C8\u3057\u3066\u3044\u307E\u3059\u304C\u3001GNU/Linux\u3067\u306F `gdb`\u3001\
  macOS\u3067\u306F `lldb` \u304C\u4E00\u822C\u7684\u3067\u3059\u3002`rust-gdb` \u3084\
  \ `rust-lldb` \u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u3082\u3067\u304D\u307E\
  \u3059\u3002\u3053\u308C\u3089\u306FRust\u306E\u5024\u3092\u304D\u308C\u3044\u306B\
  \u8868\u793A\u3059\u308B\u30E9\u30C3\u30D1\u30FC\u3067\u3059\u3002\u4E00\u4F8B\u3092\
  \u898B\u3066\u307F\u307E\u3057\u3087\u3046:\n\n```Rust\nfn main() {\n    let mut\
  \ counter = 0;\n    for _ in 0."
title: "\u30C7\u30D0\u30C3\u30AC\u30FC\u306E\u4F7F\u3044\u65B9"
weight: 35
---

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
