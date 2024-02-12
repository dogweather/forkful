---
title:                "HTMLの解析"
aliases:
- ja/rust/parsing-html.md
date:                  2024-02-03T19:13:08.782070-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTMLの解析"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
