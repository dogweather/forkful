---
title:                "文字列の長さを見つける"
html_title:           "Rust: 文字列の長さを見つける"
simple_title:         "文字列の長さを見つける"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## なぜ

文字列の長さを取得することは、プログラミングにおいて非常に一般的なタスクです。Rustを使えば、これを簡単かつ効率的に行うことができます。そのため、Rustを学ぶ上で重要なスキルの一つとなります。

## 方法

Rustで文字列の長さを取得するには、組み込みのメソッドである`len()`を使用します。以下のコードを参考にしてください。

```Rust
let my_string = String::from("こんにちは");
let length = my_string.len();
println!("文字列の長さは{}文字です。", length);
```

または、インデックスアクセスやループを使用しても同じ結果を得ることができます。

```Rust
let my_string = String::from("こんにちは");
let mut length = 0;
for _c in my_string.chars() {
    length += 1;
}
println!("文字列の長さは{}文字です。", length);
```

## 詳細について

`len()`メソッドは、文字列型に対して直接呼び出すことができる組み込みのメソッドです。これにより、メモリや文字コードの扱いについてRustが自動的に最適な方法を選択してくれます。これにより、プログラムのパフォーマンスにも影響を与えます。また、`len()`メソッドを介して取得される値は`usize`型であるため、文字列の長さがより大きな数でも適切に扱うことができます。

## 参考

- [Rust公式ドキュメント - Strings](https://doc.rust-lang.org/std/string/)
- [Rust by Example - Strings](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [Rust入門 - 文字列の扱い方](https://rustguide.readthedocs.io/ja/latest/strings.html)

## 関連リンク

- [Rust入門 - レイトレーシング](https://rustguide.readthedocs.io/ja/latest/raytracing.html)
- [Rustのライフタイム - 入門](https://qiita.com/mopp/items/3eb8a4e44ab13079d2d6)
- [Rustのデータ型 - 種類と使い方](https://qiita.com/Kaiser2981/items/f3d5d4f88f743b661d85)