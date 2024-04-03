---
date: 2024-01-26 01:11:48.114998-07:00
description: "\u65B9\u6CD5\uFF1A \u305F\u3068\u3048\u3070\u3001\u3042\u306A\u305F\u304C\
  \u8907\u6570\u56DE\u306B\u6E21\u3063\u3066\u5186\u306E\u9762\u7A4D\u3092\u8A08\u7B97\
  \u3059\u308B\u30B3\u30FC\u30C9\u3092\u6301\u3063\u3066\u3044\u305F\u5834\u5408\u3092\
  \u8003\u3048\u307E\u3057\u3087\u3046\u3002\u5F0F\u3092\u7E70\u308A\u8FD4\u3059\u4EE3\
  \u308F\u308A\u306B\u3001\u95A2\u6570\u306B\u305D\u308C\u3092\u30E9\u30C3\u30D7\u3057\
  \u307E\u3059\u3002"
lastmod: '2024-03-13T22:44:41.831653-06:00'
model: gpt-4-1106-preview
summary: "\u305F\u3068\u3048\u3070\u3001\u3042\u306A\u305F\u304C\u8907\u6570\u56DE\
  \u306B\u6E21\u3063\u3066\u5186\u306E\u9762\u7A4D\u3092\u8A08\u7B97\u3059\u308B\u30B3\
  \u30FC\u30C9\u3092\u6301\u3063\u3066\u3044\u305F\u5834\u5408\u3092\u8003\u3048\u307E\
  \u3057\u3087\u3046\u3002\u5F0F\u3092\u7E70\u308A\u8FD4\u3059\u4EE3\u308F\u308A\u306B\
  \u3001\u95A2\u6570\u306B\u305D\u308C\u3092\u30E9\u30C3\u30D7\u3057\u307E\u3059."
title: "\u30B3\u30FC\u30C9\u3092\u95A2\u6570\u306B\u6574\u7406\u3059\u308B"
weight: 18
---

## 方法：
たとえば、あなたが複数回に渡って円の面積を計算するコードを持っていた場合を考えましょう。式を繰り返す代わりに、関数にそれをラップします。

```Rust
fn calculate_circle_area(radius: f64) -> f64 {
    std::f64::consts::PI * radius.powi(2)
}

fn main() {
    let radius = 5.0;
    let area = calculate_circle_area(radius);
    println!("The area of the circle is: {}", area);
}
```

出力：

```
The area of the circle is: 78.53981633974483
```

## ディープダイブ
歴史的には、関数は数学から来ており、入力を出力にマッピングします。コーディングにおいては、アセンブリの日々から存在していますが、当時は「サブルーチン」と呼ばれていました。Rustの関数は、ファーストクラス関数とクロージャのおかげで、値や他の関数を返すことができます。

代替手段は？インラインコードやマクロがありますが、複雑なロジックには扱いづらいです。メソッドを持つオブジェクトも機能を整理する別の方法ですが、スタンドアロンの関数とは異なるフレーバーです。

Rustでの実装はかなり簡単です。関数はそのパラメータの型と返り値の型を宣言します。慣習により、関数名は「スネークケース」になります。モジュールの外部で使用するための公開関数(`pub fn`)と、内部使用のための非公開関数があります。そして、関数の最後の式に`return`キーワードを必要としないというRustのかっこいい機能もあります。

## 参考情報
さらに情報が必要な場合は、こちらをチェックしてください：
- The Rust Programming Language Book：[関数](https://doc.rust-lang.org/book/ch03-03-how-functions-work.html)
- Rust by Example：[関数](https://doc.rust-lang.org/rust-by-example/fn.html)
