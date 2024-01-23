---
title:                "正規表現の使用"
html_title:           "C: 正規表現の使用"
simple_title:         "正規表現の使用"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (何となぜ？)

正規表現はテキストのパターンを記述する。これにより、検索、置換、構文解析が容易になる。プログラマーはコードの効率と処理速度を上げるために使用する。

## How to: (方法)

Rustでは`regex`クレートを利用して正規表現を実装します。以下の例を試してみましょう。

```rust
use regex::Regex;

fn main() {
    let re = Regex::new(r"\b\w+\b").unwrap();
    let text = "The quick brown fox jumps over the lazy dog.";

    for word in re.find_iter(text) {
        println!("{}", word.as_str());
    }
}
```

実行結果は以下の通りです:

```
The
quick
brown
fox
jumps
over
the
lazy
dog
```

## Deep Dive (深掘り)

歴史的には、正規表現は1960年代に数学者スティーブン・クリーネによって提唱された。PythonやJavaScriptなど多くの言語で似たような正規表現の機能があるが、Rustの`regex`クレートは高速で安全な正規表現処理を保証している。実装上では、ライブラリはRustの所有権と借用の概念を利用してメモリ安全性を確保しつつ高速に作動する。

## See Also (関連情報)

Rustの正規表現に関するより詳細な情報:

- 公式`regex`クレートのドキュメント: [docs.rs/regex](https://docs.rs/regex/)
- Rust Bookの正規表現のセクション: [doc.rust-lang.org/book/ch18-00-patterns.html](https://doc.rust-lang.org/book/ch18-00-patterns.html)
- Rust by Exampleの正規表現チャプター: [doc.rust-lang.org/rust-by-example/std/str.html](https://doc.rust-lang.org/rust-by-example/std/str.html)
