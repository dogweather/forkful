---
title:                "正規表現の使用"
html_title:           "Rust: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

Rustで正規表現を使用するのはなぜ？

正規表現は、文字列を検索や置換する際に非常に便利です。特定のパターンを持つ文字列を効率的に抽出したり、パターンに基づいて文字列を変換したりすることができます。また、Rustの標準ライブラリに組み込まれているため、追加のライブラリをインストールする必要がありません。

## 使い方

正規表現を使用するには、まず `regex` ライブラリをインポートする必要があります。次に、`Regex` 構造体を使用してパターンをコンパイルし、`find`や`replace`などのメソッドを使用して文字列を操作します。以下は、文字列から日付を抽出する例です。

```Rust
use regex::Regex;

fn main() {

    let text = "Today is 2021/05/18";
    let re = Regex::new(r"\d{4}/\d{2}/\d{2}").unwrap();
    
    if let Some(date) = re.find(text) {
        println!("Today's date is {}", date.as_str());
    }

}
```

上記のコードを実行すると、`2021/05/18`という日付が出力されます。

## 詳細を深く掘り下げる

正規表現では、`RegexBuilder`を使用してパターンをより詳細に制御することができます。例えば、大文字と小文字を区別しないように設定したり、複数のパターンを一度にコンパイルしたりすることができます。また、`RegexSet`を使用して多くのパターンを一度に検索することもできます。

さらに、正規表現ではキャプチャーグループを使用して、特定の部分だけを抽出することもできます。キャプチャーグループは`()`の中にパターンを囲んで作成します。次の例では、日付の日、月、年をそれぞれ抽出しています。

```Rust
use regex::Regex;

fn main() {

    let text = "Today is 2021/05/18";
    let re = Regex::new(r"(\d{4})/(\d{2})/(\d{2})").unwrap();
    
    if let Some(captures) = re.captures(text) {
        println!("Year: {}", captures.get(1).unwrap().as_str());
        println!("Month: {}", captures.get(2).unwrap().as_str());
        println!("Day: {}", captures.get(3).unwrap().as_str());
    }

}
```

出力結果は以下のようになります。

```
Year: 2021
Month: 05
Day: 18
```

## 関連リンク

- Rust公式ドキュメント：https://doc.rust-lang.org/std/regex/
- 正規表現チートシート：https://www.debuggex.com/cheatsheet/regex/rust
- Rust regexクレート：https://crates.io/crates/regex