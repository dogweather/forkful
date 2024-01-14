---
title:    "Rust: テキストの検索と置き換え"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## なぜ
テキストを検索して置換する作業に取り組む理由は、プログラミングの世界ではよくあることです。例えば、大規模なプログラムで特定の変数名を一括で変更する必要がある場合や、テキスト内の特定の単語を一括で修正したい場合などがあります。Rustはこのような作業にも非常に優れたツールとなり得ます。

## 方法
Rustを使ったテキストの検索と置換は、簡単に実現できます。まずは「regex」ライブラリを使用するために、プロジェクトのCrates.tomlファイルに次の行を追加しましょう。

```
regex = "1.3.9"
```

次に、`src/main.rs`ファイルに次のようなコードを追加します。

```rust
use regex::Regex
```

そして、`main()`関数内に次のコードを記述します。

```rust
let regex = Regex::new(r"Hello").unwrap();
let replaced_text = regex.replace_all("Hello, world!", "こんにちは");
println!("{}", replaced_text);
```

コンパイルして実行すると、"こんにちは, world!"というテキストに置換されていることがわかります。

## ディープダイブ
さらに、検索パターンや置換文字列には正規表現を使用することもできます。例えば、`[A-z]`というパターンを使うことで、アルファベットの大文字と小文字を一括して置換することができます。

また、置換後のテキストにはグループを指定することもできます。例えば、`regex.replace_all("Hello, world!", "$1!")`というコードを記述することで、「Hello, world!」が「Hello!」というテキストに置換されます。

## その他の参考リンク
[Rust regexライブラリドキュメント](https://docs.rs/regex/1.3.9/regex/)
[Rustプログラミング言語公式サイト](https://www.rust-lang.org/ja/)
[Rust日本ユーザーグループ](https://rust-jp.rs/)