---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:08.782070-07:00
description: "\u2026"
lastmod: '2024-03-13T22:44:41.817953-06:00'
model: gpt-4-0125-preview
summary: "Rust\u3067HTML\u3092\u89E3\u6790\u3059\u308B\u3053\u3068\u306F\u3001HTML\u30C9\
  \u30AD\u30E5\u30E1\u30F3\u30C8\u304B\u3089\u30C7\u30FC\u30BF\u3092\u62BD\u51FA\u3059\
  \u308B\u3053\u3068\u306B\u3064\u3044\u3066\u3067\u3042\u308A\u3001\u3053\u308C\u306F\
  Web\u30B9\u30AF\u30EC\u30A4\u30D4\u30F3\u30B0\u3001\u30C7\u30FC\u30BF\u306E\u62BD\
  \u51FA\u3001\u307E\u305F\u306FWeb\u30AF\u30ED\u30FC\u30E9\u30FC\u306E\u69CB\u7BC9\
  \u306B\u4E0D\u53EF\u6B20\u3067\u3059\u3002\u30D7\u30ED\u30B0\u30E9\u30DE\u30FC\u306F\
  \u3001Web\u304B\u3089\u60C5\u5831\u3092\u81EA\u52D5\u7684\u306B\u53CE\u96C6\u3057\
  \u305F\u308A\u3001Web\u30B3\u30F3\u30C6\u30F3\u30C4\u3092\u5206\u6790\u3057\u305F\
  \u308A\u3001\u3042\u308B\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u304B\u3089\
  \u5225\u306E\u30D7\u30E9\u30C3\u30C8\u30D5\u30A9\u30FC\u30E0\u3078\u30B3\u30F3\u30C6\
  \u30F3\u30C4\u3092\u79FB\u884C\u3059\u308B\u305F\u3081\u306B\u3053\u308C\u3092\u884C\
  \u3044\u307E\u3059\u3002."
title: "HTML\u306E\u89E3\u6790"
weight: 43
---

## 何となぜ？

RustでHTMLを解析することは、HTMLドキュメントからデータを抽出することについてであり、これはWebスクレイピング、データの抽出、またはWebクローラーの構築に不可欠です。プログラマーは、Webから情報を自動的に収集したり、Webコンテンツを分析したり、あるプラットフォームから別のプラットフォームへコンテンツを移行するためにこれを行います。

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
