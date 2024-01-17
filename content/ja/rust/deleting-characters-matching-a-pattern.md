---
title:                "「パターンにマッチする文字を削除する」"
html_title:           "Rust: 「パターンにマッチする文字を削除する」"
simple_title:         "「パターンにマッチする文字を削除する」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 何 & なぜ？
「パターンにマッチする文字を削除する」とは、文字列内の特定のパターンに一致する文字を削除することを指します。プログラマーがこれを行う理由は、処理の高速化やデータの整理を目的とすることが多いです。

## 方法：
以下のように、「Rust ...」のコードブロック内にコーディングの例や出力サンプルを記載します。

```Rust
let string = "Hello World!"; // 元の文字列
let pattern = "llo"; // 削除するパターン

// 文字列からパターンに一致する文字を削除
let new_string = string.replace(pattern, "");

println!("{}", new_string); // "He World!"が出力される
```

## 深堀り：
このようなパターンにマッチする文字を削除する技術は、古くから存在していました。現在でも他の言語でも同様の機能を提供していますが、Rustは高速で安全な実行が可能です。また、より複雑なパターンに対応するための正規表現や、条件に合致する全ての文字を一度に削除することができる機能もあります。

## 関連情報：
 * [Rustの文字列操作に関する公式ドキュメント](https://doc.rust-lang.org/std/string/struct.String.html#method.replace)
 * [正規表現の基礎](https://www.atmarkit.co.jp/ait/articles/2011/25/news137.html)
 * [Rust以外の言語での文字列操作の比較](https://qiita.com/takl/items/986c041c6425e8b47c30)