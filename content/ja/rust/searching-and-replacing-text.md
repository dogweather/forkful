---
title:                "Rust: テキストの検索と置換"
simple_title:         "テキストの検索と置換"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## なぜ

テキストの検索と置換をすることは、コンピューターのテキスト処理の重要な部分です。Rustプログラミング言語を使うことで、高速かつ安全にテキストを置換することができます。

## 方法

まず、"regex"というモジュールを使用して、テキストを検索するパターンを定義します。例えば、"hello"という単語を検索する場合、`regex::Regex::new("hello")`というコードを使います。次に、検索したいテキストを`find`メソッドで指定します。最後に、`replace`メソッドを使って、見つかったテキストを置換することができます。

以下は、"hello Rust"というテキストが含まれた文章を検索して、"こんにちはRust"に置換する例です。

```Rust
use regex::Regex;

let pattern = Regex::new("hello").unwrap();
let text = "This is a sentence with hello Rust in it.";
let result = pattern.replace(text, "こんにちはRust");
```

結果は以下のようになります。

> This is a sentence with こんにちはRust in it.

## ディープダイブ

Rustでは、"regex"モジュールのほかにもさまざまなツールを使ってテキストの検索と置換を行うことができます。例えば、ファイルの内容を検索する場合には"grep"ツールを使うこともできます。また、多言語対応やパフォーマンスの最適化など、さまざまなオプションがありますので、興味がある方はさらに詳しく調べてみてください。

## 参考リンク

- [Rustでテキスト検索と置換を行う](https://dev.to/ryanmcdermott/search-and-replace-text-in-rust-3eod)
- [regexパッケージドキュメント](https://docs.rs/regex/1.3.1/regex/)
- [grepツールのドキュメント](https://github.com/BurntSushi/ripgrep#flexible-pattern-syntax)