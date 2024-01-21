---
title:                "部分文字列の抽出"
date:                  2024-01-20T17:45:33.647239-07:00
model:                 gpt-4-1106-preview
simple_title:         "部分文字列の抽出"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)
コードで部分文字列を取り出すって何？使う理由は？部分文字列を取り出すってのは、文字列の中から特定の部分だけを選んで切り出すことだ。データを整形したり、特定の情報を得るために使うんだ。

## How to: (方法)
```gleam
import gleam/string

fn main() {
  let text = "こんにちは、グリームの世界へ！"
  let hello = string.slice(text, 0, 5)  // "こんにちは"

  // コンソールに表示して確認
  io.debug(hello)  // "こんにちは" って出力される
}
```

## Deep Dive (掘り下げ)
部分文字列を取り出す方法は昔からある。Gleamでは`string.slice`関数が使えるが、他の言語では同じ操作が異なる名前で提供されることがある（例：Pythonの`slice()`、Javaの`substring()`）。Gleamでは文字列はUTF-8エンコードされていて、`slice`は文字単位（バイト単位ではない）で操作するから、マルチバイト文字も安全に扱える。

## See Also (関連情報)
- [Programming Phoenix (for inspiration from Elixir)](https://pragprog.com/titles/phoenix14/programming-phoenix-1-4/)
- [Rust by Example (to compare with Rust's approach to string slicing)](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)