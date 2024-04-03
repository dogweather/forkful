---
date: 2024-01-20 17:53:35.645223-07:00
description: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3063\u3066\u306E\u306F\u3001\u30B3\
  \u30FC\u30C9\u306E\u52D5\u304D\u3092\u898B\u3084\u3059\u304F\u3059\u308B\u305F\u3081\
  \u306B\u4F7F\u3046\u3082\u3093\u3067\u3059\u3002\u554F\u984C\u89E3\u6C7A\u3084\u30B3\
  \u30FC\u30C9\u7406\u89E3\u306B\u5F79\u7ACB\u3064\u3093\u3060\u3002"
isCJKLanguage: true
lastmod: '2024-03-13T22:44:41.826085-06:00'
model: gpt-4-1106-preview
summary: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3063\u3066\u306E\u306F\u3001\u30B3\
  \u30FC\u30C9\u306E\u52D5\u304D\u3092\u898B\u3084\u3059\u304F\u3059\u308B\u305F\u3081\
  \u306B\u4F7F\u3046\u3082\u3093\u3067\u3059\u3002\u554F\u984C\u89E3\u6C7A\u3084\u30B3\
  \u30FC\u30C9\u7406\u89E3\u306B\u5F79\u7ACB\u3064\u3093\u3060\u3002."
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

## What & Why? (何となぜ？)
デバッグ出力ってのは、コードの動きを見やすくするために使うもんです。問題解決やコード理解に役立つんだ。

## How to: (やり方)
```Rust
fn main() {
    let my_var = vec![1, 2, 3];
    println!("Debug output: {:?}", my_var); // 標準的なデバッグ出力
}

```
出力結果:
```
Debug output: [1, 2, 3]
```

上級者向け:
```Rust
fn main() {
    let my_struct = MyStruct { x: 10, y: 20 };
    println!("Debug output: {:#?}", my_struct); // より読みやすい形式
}

#[derive(Debug)]
struct MyStruct {
    x: i32,
    y: i32,
}
```
出力結果:
```
Debug output: MyStruct {
    x: 10,
    y: 20,
}
```

## Deep Dive (踏み込んだ内容)
デバッグ出力は古くから開発者の間で使われてる。昔は`print`文だけだったけど、Rustでは`println!`マクロを使う。これにより、フォーマットされた出力が可能。

代わりに`log`クレートを使う手もある。これは、異なるログレベルでの出力管理を可能にする。

デバッグ出力では`{:?}`や`{:#?}`プレースホルダーが使われる。対象の型に`Debug`トレイトが実装されてないといけない。自分で作った構造体には、`#[derive(Debug)]`アトリビュートを追加する必要があるんだ。

## See Also (関連情報)
- [The Rust Programming Language book - Printing with `{:?}` and `{:#?}`](https://doc.rust-lang.org/book/ch05-02-example-structs.html#adding-useful-functionality-with-derived-traits)
- [Rust by Example - Debug trait](https://doc.rust-lang.org/rust-by-example/hello/print/print_debug.html)
