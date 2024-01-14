---
title:                "Rust: 「文字列の小文字への変換」"
simple_title:         "「文字列の小文字への変換」"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# なぜRustで文字列を小文字に変換する必要があるのか
文字列の処理において、大文字や小文字を区別することが必要な場合があります。例えば、入力された文字列がユーザーのパスワードと一致するかどうかをチェックする際には、大文字小文字を区別せずに比較する必要があります。そのような場合には、文字列をすべて小文字に変換する必要があります。

## 方法
Rustでは、標準ライブラリのString型に対してメソッドを使用することで、文字列を小文字に変換することができます。以下に例を示します。

```Rust
let input = String::from("Hello World!");
let output = input.to_lowercase();
println!("{}", output); // Output: hello world!
```

上記の例では、String型の変数に対して`to_lowercase`メソッドを使用しています。このメソッドは文字列を小文字に変換した結果を返します。変換後の文字列は別の変数に格納することで、もとの文字列を変更せずに小文字に変換することができます。

## 深堀り
Rustでは、文字列を表すString型はイミュータブル (immutable) であるため、変数に格納された文字列を直接編集することはできません。そのため、文字列の小文字化を行う際には新しい文字列を作成する必要があります。

文字列の小文字化を行う際には、Unicodeの規則に従って変換が行われます。つまり、アルファベットやアクセント記号などの文字が小文字に変換されますが、数字や記号は変化しません。また、Unicodeには特殊な文字が存在するため、文字列を小文字に変換する際には不可解な変換結果が得られる場合があります。そのため、小文字変換の際には必ず結果を確認するようにしましょう。

## もっと詳しく知りたい方へ
- [Rustの公式ドキュメント - 文字列型のメソッド](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Rust by Example - 文字列の扱い](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [RustコンパイラとUnicodeの問題](https://qiita.com/Quii/items/5541b56abecb5c8adf51)（日本語記事）
- [Unicodeの公式サイト](https://unicode.org/)（英語）


# 参考リンク

- [Rustの公式ドキュメント - 文字列型のメソッド](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- [Rust by Example - 文字列の扱い](https://doc.rust-lang.org/rust-by-example/std/str.html)
- [RustコンパイラとUnicodeの問題](https://qiita.com/Quii/items/5541b56abecb5c8adf51)（日本語記事）
- [Unicodeの公式サイト](https://unicode.org/)（英語）