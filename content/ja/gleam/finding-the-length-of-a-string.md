---
title:                "文字列の長さを求める"
date:                  2024-01-20T17:47:19.586291-07:00
model:                 gpt-4-1106-preview
simple_title:         "文字列の長さを求める"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
文字列の長さを測るとは、与えられたテキストの文字数を数えることです。これをプログラマーが行う理由は、テキスト処理や検証、UIレイアウトの最適化などを行う際に必要だからです。

## How to:
```gleam
pub fn main() {
  let greeting = "こんにちは"
  let length = string.len(greeting)
  io.println(length)
}
```
出力は `5`です。 (`こんにちは`の文字数)

## Deep Dive
Gleam言語では、文字列の長さを簡単に見つけることができ、`string.len`関数を使用します。ここでの「長さ」とは、文字列内の文字の数を意味し、Gleamの前身であるErlangのビルトイン機能を利用しています。他のプログラミング言語と異なり、GleamはUTF-8エンコーディングされた文字列を扱い、全ての文字はUnicodeとして正しくカウントされます。他の言語では、特殊なエンコーディングまたは異なる文字カウント方法が提供されることがありますが、Gleamは開発者に明確で一貫した挙動を提供します。

## See Also
- Gleam公式ドキュメント: [https://gleam.run/](https://gleam.run/)
- Unicodeについて: [https://unicode.org/](https://unicode.org/)
- Erlangの文字列処理: [http://erlang.org/doc/apps/stdlib/unicode_usage.html](http://erlang.org/doc/apps/stdlib/unicode_usage.html)