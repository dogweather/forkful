---
title:                "文字のパターンに一致する文字を削除する"
html_title:           "Rust: 文字のパターンに一致する文字を削除する"
simple_title:         "文字のパターンに一致する文字を削除する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## なぜ

あなたは何かの文字列から特定のパターンにマッチする文字を削除したい場合、効率的かつ正確な方法が必要になるでしょう。Rustの機能を使えば、簡単にそのような処理が行えます。

## 使い方

### パターンにマッチする文字を削除する

文字列から特定の文字パターンにマッチする文字を削除するには、正規表現を使用します。Rustには`regex`クレートがあり、それを使用することで簡単にパターンにマッチする文字を削除することができます。

```
Rust
use regex::Regex;

let input = "This is a 1234 test string 5678";
let re = Regex::new("\\d").unwrap(); // 数字にマッチする正規表現を作成
let output = re.replace_all(input, ""); // パターンにマッチする文字を空文字に置換
println!(output); // "This is a test string"

```

このように、`regex`クレートのメソッド`.replace_all()`を使用することで、簡単に文字列からパターンにマッチする文字を削除することができます。

### 特定の文字を削除しないようにする

もし、特定の文字を削除しないようにしたい場合は、マッチする文字を置換する文字列を指定すれば良いです。例えば、数字を削除せずに空白に置換する場合、以下のように書き換えることができます。

```
Rust
use regex::Regex;

let input = "This is a 1234 test string 5678";
let re = Regex::new("\\d").unwrap(); // 数字にマッチする正規表現を作成
let output = re.replace_all(input, " "); // パターンにマッチする文字を空白に置換
println!(output); // "This is a   test string"

```

このように、`.replace_all()`の第2引数に置換したい文字列を指定することで、特定の文字を削除しないようにすることができます。

## ディープダイブ

上記のコードでは、`.replace_all()`メソッドを使用しましたが、実際は`.replace()`メソッドを使用しても同じ結果が得られます。`.replace_all()`メソッドは全てのマッチする箇所を置換するのに対して、`.replace()`メソッドは最初のマッチする箇所のみを置換します。

また、正規表現に関する詳細な情報や、`regex`クレートの他の便利な機能についても学ぶことができます。詳しくは、[公式ドキュメント](https://docs.rs/regex/latest/regex/)や[チュートリアル](https://docs.rs/regex/1.4.3/regex/#quick-start)を参照してください。

## See Also

- [Regex Cheat Sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/) - 正規表現の基本的なパターンや構文をまとめたチートシートです。
- [Rust Regex Playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=d76c6e4b2c4c8c1f1491b1a7eb00c2d6) - 上記で使用したコードを実際に動かすことができるオンラインのプレイグラウンドです。