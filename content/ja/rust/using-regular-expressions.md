---
title:                "正規表現を使用する"
html_title:           "Rust: 正規表現を使用する"
simple_title:         "正規表現を使用する"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 何やっているの? & なぜ必要なの?
正規表現を使うことは、特定のパターンを持つテキストを検索、抽出、置換するための方法です。プログラマーたちは、文字列処理に役立つ強力なツールとして正規表現を使用します。

## 使い方:
```Rust 
fn main() {
    // 文字列から単語を抽出する
    let input = "今日はいい天気だ！";
    let re = Regex::new(r"\w+").unwrap();

    for cap in re.captures_iter(input) {
        println!("{}", cap.get(0).unwrap().as_str());
    }
}
```
出力:
今日
は
いい
天気
だ

## 深堀り:
正規表現は、1960年代にテキスト処理のために開発されました。他の代替手段として、文字列メソッドやループ処理を使用することもできますが、正規表現はより簡潔で効率的なコードを書くことができます。Rustでは、Regexライブラリを使用して正規表現を実装することができます。

## リンク:
- [Rust Regexライブラリドキュメント](https://docs.rs/regex/1.5.4/regex/)
- [正規表現チュートリアル（日本語）](https://www.javadrive.jp/regex/)