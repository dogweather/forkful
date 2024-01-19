---
title:                "部分文字列の抽出"
html_title:           "Lua: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## 何となんで？

サブストリングの抽出は、大きな文字列から特定の部分を取り出すことです。プログラマーがこれを行うのは、テキストデータの操作や特定の部分の検査・分析が必要なためです。

## どうやる:

以下にRustプログラミング言語でのサブストリングの抽出方法を示します。

```Rust
fn main() {
    let s = "Hello, world!";
    let substring = &s[0..5];
    println!("{}", substring);
}
```

出力例

```Rust
Hello
```

上記は文字列 'Hello, world!'から最初の5文字 'Hello' を抽出する例です。

## 深掘り：

サブストリングの抽出はコンピューターサイエンスの初期からある概念で、テキスト操作タスクを効率的に行うのに不可欠です。Rustではスライス構文を使ってこの操作を行いますが、他の言語では異なるメソッドが使われることがよくあります。

例えば、Pythonではスライスだけでなく、メソッド `substring()` や正規表現も強力な抽出ツールとして使われます。逆に、Javaでは `substring()` メソッドが主に使われます。

Rustでサブストリングを抽出するためには、指定した範囲のコピーではなく、元の文字列への参照を取得することを理解するのが重要です。これにより、メモリ効率を確保しています。

## 参考になる：

以下に関連するリンクを示します：
* [Rustプログラミング入門](https://stevedonovan.github.io/rustifications/2018/09/08/common-rust-lifetime-misconceptions.html)
* [Rustでの文字列スライシング](https://doc.rust-lang.org/book/ch08-02-strings.html)