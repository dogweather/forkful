---
title:                "文字列を小文字に変換する"
html_title:           "Arduino: 文字列を小文字に変換する"
simple_title:         "文字列を小文字に変換する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 何となぜ？
文字列を小文字に変換するということは、すべての文字を小文字に変えることです。この手段は、大文字小文字を区別せずに比較や検索を行うためにプログラミングでよく使われます。

## どのように：
Rustには文字列を小文字に変換するメソッドとして`to_lowercase`があります。
```Rust
fn main() {
    let s = "Hello, RUST!".to_lowercase();
    println!("{}", s);
}
```
このコードは"hello, rust!"と出力します。

## 詳細：
1. 歴史的な背景：以前のプログラミング言語、例えばCやJavaでは、文字列を小文字に変換するためにはループを使うことが一般的でした。しかし、Rustはこれを独自のメソッドとして提供することで、より直感的で簡単に文字列の小文字変換ができるようになりました。

2. 代替手段：特定の条件下でのみ小文字に変換する場合など、`to_lowercase` 以外の方法を必要とする場合もあります。そのような場合には、Rustのパターンマッチングや`chars`メソッドと`match`文を使って自己実装が可能です。

3. 実装詳細：`to_lowercase`メソッドは`char`ごとに機能します。それぞれの`char`を小文字に変換し、その結果を連結して新たな`String`を生成します。

## 参考資料：
- Rustの公式ドキュメンテーション: [https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- Rustのパターンマッチングについて: [https://doc.rust-lang.org/book/ch06-02-match.html](https://doc.rust-lang.org/book/ch06-02-match.html)