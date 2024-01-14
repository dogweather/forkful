---
title:                "Rust: 「パターンにマッチする文字の削除」"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# なぜ？（Why）

あなたがプログラミングをしているときに、文字のパターンにマッチする文字を削除したいと思うことはよくあります。例えば、文字列からすべての数字を削除したい場合などがあります。そんなときに、Rustプログラミング言語でどのようにして文字を削除するかについて学びましょう。

## 方法（How To）

Rustでは、特定のパターンにマッチする文字を削除するために`replace`メソッドを使用します。以下のコード例を参考にしてみてください。

```Rust
fn main() {
    let word = "Hello,123World!";
    let new_word = word.replace(char::is_numeric, ""); // 数字を含む文字をすべて削除
    println!("{}", new_word); // => Hello,World!
}
```

このコードでは、`replace`メソッドを使用して文字列`Hello,123World!`から数字を含む文字をすべて削除し、新しい文字列`Hello,World!`を作成しています。このように、`replace`メソッドを使うことで簡単に文字を削除することができます。

## 詳細（Deep Dive）

`replace`メソッドは、文字列以外にも`char`や`&str`でも使用することができます。また、正規表現を使用して複雑なパターンにもマッチすることができます。さらに、`replace`メソッドは新しい文字列を返すだけでなく、元の文字列を変更することもできます。詳細については、公式ドキュメントを参考にしてください。

## See Also

- [Rust公式ドキュメント：`replace`メソッド]（https://doc.rust-lang.org/std/string/struct.String.html#method.replace）
- [Rustでの正規表現の使用方法]（https://tech-blog.optim.co.jp/entry/2018/09/06/080000）