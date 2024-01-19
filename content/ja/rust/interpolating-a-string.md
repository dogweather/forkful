---
title:                "文字列の補間"
html_title:           "Arduino: 文字列の補間"
simple_title:         "文字列の補間"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Rustでの文字列補間の理論と実践
こんにちは、Rustをやってみますか？今日は文字列補間がどんなもので、なぜそれが必要なのか、そしてどのようにRustでそれを行うかを非常に簡単に解説します。

## 何？そしてなぜ？
文字列補間とは、基本的には文字列中の特定の部分をプログラムの変数や式で置き換える能力のことを指します。これは非常に頻繁にコードの出力を整形するために利用されます。

```Rust
let name = "Taro";
println!("Hello, {}", name);
```
出力: `Hello, Taro`

## 実行方法：
Rustでは、以下のように`format!`マクロを使用することで文字列補間を直接実現することができます。

```Rust
let world = "世界";
let greeting = format!("こんにちは, {}!",world);
println!("{}", greeting);
```
出力: `こんにちは, 世界!`

## 深掘り
文字列補間は、他の多くのハイレベルなプログラミング言語、例えばperlやrubyなどで見つけることができます。それらの言語では、変数や式が冗長な構文を排除して直接文字列に組み込まれます。影響を受けた他の言語とは異なり、Rustはコンパイル時に型安全を保証することに重点を置くため、構文は少し異なります。

もちろん、可読性を向上させるためのいくつかの代替手段があります。例えば、記号`+`を使用した文字列連結が挙げられますが、一般的にこの方法はコードのパフォーマンスに悪影響を及ぼします。

この実装は、Rustが型安全の言語であることを反映しており、全ての型が`{}`プレースホルダーに適用できるとは限らないため、特定の型に対しては自分でその型の文字列表現を定義する必要があります。

## 参考資料
- [公式ドキュメントの文字列](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Rust Bookによるformat!の解説](https://doc.rust-lang.org/stable/book/ch03-05-control-flow.html?highlight=format#function-parameters)

以上です。Rustで素晴らしいコードを書くための旅を楽しんで下さい！