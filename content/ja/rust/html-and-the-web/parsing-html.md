---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:08.782070-07:00
description: "\u65B9\u6CD5: Rust\u3067HTML\u3092\u89E3\u6790\u3059\u308B\u306B\u306F\
  \u3001`scraper`\u30AF\u30EC\u30FC\u30C8\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\
  \u304C\u3088\u304F\u3042\u308A\u307E\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001\
  HTML\u30C9\u30AD\u30E5\u30E1\u30F3\u30C8\u3092\u30C8\u30E9\u30D0\u30FC\u30B9\u3057\
  \u3001\u64CD\u4F5C\u3059\u308B\u305F\u3081\u306E\u9AD8\u30EC\u30D9\u30EB\u306E\u30A4\
  \u30F3\u30BF\u30FC\u30D5\u30A7\u30A4\u30B9\u304C\u63D0\u4F9B\u3055\u308C\u307E\u3059\
  \u3002 \u307E\u305A\u3001`Cargo.toml`\u306B`scraper`\u3092\u8FFD\u52A0\u3057\u307E\
  \u3059\uFF1A."
lastmod: '2024-04-05T22:37:50.106065-06:00'
model: gpt-4-0125-preview
summary: "Rust\u3067HTML\u3092\u89E3\u6790\u3059\u308B\u306B\u306F\u3001`scraper`\u30AF\
  \u30EC\u30FC\u30C8\u3092\u4F7F\u7528\u3059\u308B\u3053\u3068\u304C\u3088\u304F\u3042\
  \u308A\u307E\u3059\u3002\u3053\u308C\u306B\u3088\u308A\u3001HTML\u30C9\u30AD\u30E5\
  \u30E1\u30F3\u30C8\u3092\u30C8\u30E9\u30D0\u30FC\u30B9\u3057\u3001\u64CD\u4F5C\u3059\
  \u308B\u305F\u3081\u306E\u9AD8\u30EC\u30D9\u30EB\u306E\u30A4\u30F3\u30BF\u30FC\u30D5\
  \u30A7\u30A4\u30B9\u304C\u63D0\u4F9B\u3055\u308C\u307E\u3059\u3002"
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

## 方法:
RustでHTMLを解析するには、`scraper`クレートを使用することがよくあります。これにより、HTMLドキュメントをトラバースし、操作するための高レベルのインターフェイスが提供されます。

まず、`Cargo.toml`に`scraper`を追加します：

```toml
[dependencies]
scraper = "0.12.0"
```

次に、与えられたHTML文字列からすべてのリンクURLを抽出する簡単な例を示します：

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">リンク1</a>
        <a href="http://example.com/2">リンク2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("発見されたリンク: {}", link);
    }
}
```

出力：

```
発見されたリンク: http://example.com/1
発見されたリンク: http://example.com/2
```

この例では、単純なHTML文書を解析してすべての`<a>`要素を見つけ、それらの`href`属性を抽出することで、文書内のすべてのリンクのURLを効果的に印刷しています。`scraper`ライブラリは、HTMLの解析とCSSセレクタを使用して特定の要素を選択することを簡略化し、RustでのWebスクレイピングタスクに最適です。
