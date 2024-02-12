---
title:                "コードを関数に整理する"
date:                  2024-01-26T01:11:48.114998-07:00
model:                 gpt-4-1106-preview
simple_title:         "コードを関数に整理する"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## はじめに：その理由とは？
コードを関数に整理するということは、名前で識別される再利用可能でモジュラーなチャンクにプログラムを分割することです。私たちがそれを行う理由は、コードをよりクリーンに、読みやすく、デバッグしやすくするためです。それは自分自身を繰り返さないこと、そして更新を合理化することに関しています。

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
