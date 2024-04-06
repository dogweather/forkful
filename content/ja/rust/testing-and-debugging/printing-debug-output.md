---
date: 2024-01-20 17:53:35.645223-07:00
description: "How to: (\u3084\u308A\u65B9) \u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\
  \u53E4\u304F\u304B\u3089\u958B\u767A\u8005\u306E\u9593\u3067\u4F7F\u308F\u308C\u3066\
  \u308B\u3002\u6614\u306F`print`\u6587\u3060\u3051\u3060\u3063\u305F\u3051\u3069\u3001\
  Rust\u3067\u306F`println!`\u30DE\u30AF\u30ED\u3092\u4F7F\u3046\u3002\u3053\u308C\
  \u306B\u3088\u308A\u3001\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3055\u308C\u305F\u51FA\
  \u529B\u304C\u53EF\u80FD\u3002 \u4EE3\u308F\u308A\u306B`log`\u30AF\u30EC\u30FC\u30C8\
  \u3092\u4F7F\u3046\u624B\u3082\u3042\u308B\u3002\u3053\u308C\u306F\u3001\u7570\u306A\
  \u308B\u30ED\u30B0\u30EC\u30D9\u30EB\u3067\u306E\u51FA\u529B\u7BA1\u7406\u3092\u53EF\
  \u80FD\u306B\u3059\u308B\u3002\u2026"
isCJKLanguage: true
lastmod: '2024-04-05T22:37:50.111932-06:00'
model: gpt-4-1106-preview
summary: "(\u3084\u308A\u65B9) \u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u306F\u53E4\u304F\
  \u304B\u3089\u958B\u767A\u8005\u306E\u9593\u3067\u4F7F\u308F\u308C\u3066\u308B\u3002\
  \u6614\u306F`print`\u6587\u3060\u3051\u3060\u3063\u305F\u3051\u3069\u3001Rust\u3067\
  \u306F`println!`\u30DE\u30AF\u30ED\u3092\u4F7F\u3046\u3002\u3053\u308C\u306B\u3088\
  \u308A\u3001\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u3055\u308C\u305F\u51FA\u529B\u304C\
  \u53EF\u80FD\u3002"
title: "\u30C7\u30D0\u30C3\u30B0\u51FA\u529B\u3092\u8868\u793A\u3059\u308B"
weight: 33
---

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
