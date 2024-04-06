---
date: 2024-01-26 00:57:33.623463-07:00
description: "\u3069\u306E\u3088\u3046\u306B\uFF1A Rust\u306F\u4E3B\u306B2\u3064\u306E\
  \u65B9\u6CD5\u3067\u30A8\u30E9\u30FC\u3092\u51E6\u7406\u3057\u307E\u3059\uFF1A\u56DE\
  \u5FA9\u53EF\u80FD\u30A8\u30E9\u30FC\u3068\u56DE\u5FA9\u4E0D\u53EF\u80FD\u30A8\u30E9\
  \u30FC\u3067\u3059\u3002\u4E21\u65B9\u306B\u3064\u3044\u3066\u898B\u3066\u307F\u307E\
  \u3057\u3087\u3046\u3002 \u56DE\u5FA9\u53EF\u80FD\u30A8\u30E9\u30FC\u3067\u306F\
  `Result<T, E>`\u3092\u4F7F\u7528\u3057\u307E\u3059\uFF1A."
lastmod: '2024-04-05T21:53:42.733974-06:00'
model: gpt-4-1106-preview
summary: "\u56DE\u5FA9\u53EF\u80FD\u30A8\u30E9\u30FC\u3067\u306F`Result<T, E>`\u3092\
  \u4F7F\u7528\u3057\u307E\u3059\uFF1A."
title: "\u30A8\u30E9\u30FC\u51E6\u7406"
weight: 16
---

## どのように：
Rustは主に2つの方法でエラーを処理します：回復可能エラーと回復不可能エラーです。両方について見てみましょう。

回復可能エラーでは`Result<T, E>`を使用します：

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("ファイルのオープンに成功しました。"),
        Err(_e) => println!("ファイルを開くのに失敗しました。"),
    }
}
```

出力は`hello.txt`の状態に応じて「ファイルのオープンに成功しました。」または「ファイルを開くのに失敗しました。」のどちらかです。

回復不可能エラーの場合には`panic!`を使います：

```Rust
fn main() {
    // これはファイルが存在しないため、プログラムをパニックさせるでしょう。
    let _f = File::open("nowhere.txt").unwrap();
}
```

実行すると、パニックメッセージが表示されます。プログラムは即座に停止します。

## 深掘り
歴史的に見て、プログラミングにおけるエラーハンドリングは混乱をきたしていました。Rustは、回復可能エラーと回復不可能エラーの明確な区別を持っており、それを正しく取り入れています。

回復可能エラー用の`Result`列挙型は明示的です。`Ok`または`Err`のバリアントを処理します。`unwrap()`や`expect()`のようなメソッドもありますが、それらは`panic!`に繋がり得る速攻で汚い近道です。

`panic!`は、本当に悪いことが起こったとRustが叫んでいる方法であり、対処不可能です。プログラムの実行を即座に停止する、回復不可能なエラーのようなものです。Rustにおけるパニックは、配列の境界外へのインデックス付けのような、処理を予期していないバグでしばしば感じられます。

エラーを処理するために`Result`を返すことは、エラーを処理することを期待している場合に推奨されます。それは慣用的なRustであり、つまりRust開発者が物事を行う方法として合意したものです。エラーが単に`None`の代わりに`Some(T)`である場合の`Option<T>`もあります。恐れることなく、予期せぬ状況を期待することについてです。

代替手段？もちろん、より多くの機能や使いやすさのために他のエラーハンドリングクレートを使用することもできます。たとえば、シンプルなエラーハンドリングには`anyhow`を、ライブラリコードのエラーには`thiserror`を使用します。

## 参照
さらに深く掘り下げたいですか？次を参照してください：

- [Rust Book on Error Handling](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Rustのエラーハンドリングの哲学を理解するのに最適な場所です。
- [Rust by Example: Error handling](https://doc.rust-lang.org/rust-by-example/error.html) - 実際に手を動かし経験を積めるインタラクティブな例です。

良いエラーハンドリングは単にコーティングすることだけではありません。それは、コードの使用者に対する思いやりです。ハッピーコーディング！
