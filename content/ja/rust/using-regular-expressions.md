---
title:                "Rust: 正規表現の利用方法"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## なぜ

正規表現を使用する理由を簡単に説明します。正規表現は、文字列のパターンを検索、抽出、置換するために使用される強力なツールです。

## 使い方

正規表現はRustの標準ライブラリであるregexクレートを使用して実装することができます。以下のコード例では、文字列の中から特定のパターンにマッチする文字列を検索し、抽出する方法を示します。

```Rust
use regex::Regex;

fn main() {
    // 例: 電話番号の抽出
    let re = Regex::new(r"\d{3}-\d{4}-\d{4}").unwrap(); // マッチするパターンを定義
    let text = "私の電話番号は012-3456-7890です"; // 検索対象の文字列
    for caps in re.captures_iter(text) { // マッチした文字列をイテレーターで取得
        println!("電話番号が見つかりました: {}", caps.get(0).unwrap().as_str());
    }
}
```

上記のコードは次のような出力を生成します。

```Rust
電話番号が見つかりました: 012-3456-7890
```

さらに、正規表現によって文字列を置換することもできます。次のコードでは電話番号をマスキングして出力します。

```Rust
use regex::Regex;

fn main() {
    // 例: 電話番号のマスキング
    let re = Regex::new(r"\d{3}-\d{4}-\d{4}").unwrap(); // マッチするパターンを定義
    let text = "私の電話番号は012-3456-7890です"; // 検索対象の文字列
    let masked = re.replace_all(text, "XXX-XXXX-XXXX"); // マッチした文字列を置換
    println!("{}", masked); // 出力: 私の電話番号はXXX-XXXX-XXXXです
}
```

## 深堀り

正規表現は文字列のパターンマッチングを行うため、特定の行為に特化した独自の言語機能を提供します。Rustでは、regexクレートを使用することで、シンプルかつ効率的な正規表現を実装することができます。

## 参考リンク

- [Rustの正規表現チュートリアル](https://github.com/rust-lang/regex/blob/master/examples/tutorial.md)
- [Rust crate: regex](https://crates.io/crates/regex)
- [正規表現速習講座](https://www.javadrive.jp/regex/index1.html)

## さらに見る

- [Rustの文字列操作ガイド](https://doc.rust-lang.org/book/ch08-03-hash-maps.html#summary)
- [Rustの標準ライブラリリファレンス](https://doc.rust-lang.org/std/str/index.html)