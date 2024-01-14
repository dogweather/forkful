---
title:                "Rust: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使用する理由は、テキストデータ内の特定のパターンを検索したり置換したりするために便利です。また、データ検証やフォーマットの検証など、さまざまな用途に使用することができます。

## 使い方

正規表現を使用するには、まずはRustの"regex"クレートをインストールする必要があります。コマンドラインで以下のように入力します。

```
cargo install regex
```

次に、作業ディレクトリに移動し、"main.rs"という新しいファイルを作成します。そして、以下のようなコードを書きます。

```rust
// 必要なモジュールのインポート
use regex::Regex;

fn main() {
    // 正規表現のパターンを定義
    let pattern = Regex::new(r"Rust").unwrap();

    // 検索対象のデータを定義
    let data = "Hello, I love Rust programming language!";

    // マッチする部分を全て取得する
    let matches = pattern.find_iter(data);

    // マッチした部分を表示する
    for mat in matches {
        println!("Found match: {}", mat.as_str());
    }

    // マッチした部分を"Java"に置換する
    let replaced_data = pattern.replace_all(data, "Java");
    
    // 置換後のデータを表示する
    println!("Replaced data: {}", replaced_data);
}
```

上のコードを実行すると、以下のような出力が得られます。

```
Found match: Rust
Replaced data: Hello, I love Java programming language!
```

このように、正規表現を使用することで、様々な応用が可能です。

## ディープダイブ

正規表現を使用する際には、いくつかのパターンや特殊文字に注意する必要があります。

まず、`r`を前置することで、文字列を生のまま取得することができます。これを使用することで、エスケープ文字の処理を簡略化することができます。

また、`?`, `+`, `*`などの特殊文字を使用することで、パターンのマッチングをさらに強化することができます。これらの文字は、それぞれ直前の検索対象が1回、1回以上、0回以上の繰り返しであることを意味します。

さらに、キャプチャーグループを作成することで、マッチした部分を後から取り出すことができます。例えば、`r"(?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})"`というパターンを使用すると、"2021-01-01"という文字列から年、月、日をそれぞれ抽出することができます。

正規表現を使用する際には、公式ドキュメントを参考にして、様々なパターンを試してみることをおすすめします。

## 他に見るもの

- [Rustで正規表現を使用する方法](https://doc.rust-lang.org/std/vec/struct.Vec.html)
- [正規表現クイックチュートリアル](https://qwtel.com/posts/software/regular-expressions/)
- [正規表現のパターン検索コレクション](https://github.com/rust-lang/regex/blob/master/examples/search.rs