---
title:                "部分文字列の抽出"
html_title:           "Rust: 部分文字列の抽出"
simple_title:         "部分文字列の抽出"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

＃＃なぜ？
抽出されたサブストリングに注目するのはなぜでしょうか？まず、サブストリングを抽出することで、文字列のある部分を分離して扱いやすくすることができます。また、サブストリングを使用することで、より複雑な文字列操作をより効率的に行うことができます。

＃＃ 手順
サブストリングの抽出方法を見てみましょう。まず、元の文字列を指定します。それから、何文字目から何文字分を抽出するかを指定します。ここでは、```Rust```のコードブロックを使用して、実際のコード例を紹介します。

```
let str = "Hello, world!";
let substr = &str[2..8];
println!("{}", substr);

// Output: llo, w
```

このように、抽出したい文字の開始位置と終了位置を指定することで、サブストリングを簡単に取得することができます。

＃＃ディープダイブ
サブストリングの抽出には、一見シンプルな方法がありますが、実際はさまざまなテクニックがあります。例えば、特定の文字を基準としてサブストリングを抽出する方法や、正規表現を使用して特定のパターンにマッチする文字列を抽出する方法などがあります。また、一度に複数のサブストリングを抽出する方法もあります。これらのテクニックをマスターすることで、より効率的に文字列を操作することができます。

＃＃参考リンク
- [Rust公式ドキュメント (日本語)](https://doc.rust-jp.rs/book/second-edition/ch04-03-slices.html)
- [サブストリングを抽出する方法 (英語)](https://www.geeksforgeeks.org/rust-programming-extracting-substring-a-string/)
- [正規表現を使用してサブストリングを抽出する方法 (英語)](https://doc.rust-lang.org/std/str/struct.SubStr.html)