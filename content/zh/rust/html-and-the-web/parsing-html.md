---
title:                "解析HTML"
aliases:
- /zh/rust/parsing-html.md
date:                  2024-02-03T19:12:57.137642-07:00
model:                 gpt-4-0125-preview
simple_title:         "解析HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/rust/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 什么 & 为什么？

在 Rust 中解析 HTML 意味着从 HTML 文档中抽取数据，这对于网页抓取、数据提取或构建网页爬虫至关重要。程序员这样做是为了自动化从网络收集信息、分析网络内容或将内容从一个平台迁移到另一个平台。

## 如何做：

要在 Rust 中解析 HTML，你通常会使用 `scraper` 库，它提供了一个高级接口来遍历和操作 HTML 文档。

首先，将 `scraper` 添加到你的 `Cargo.toml` 中：

```toml
[dependencies]
scraper = "0.12.0"
```

接下来，这里有一个简单的示例，它从给定的 HTML 字符串中提取所有链接 URL：

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">链接 1</a>
        <a href="http://example.com/2">链接 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("找到链接: {}", link);
    }
}
```

输出：

```
找到链接: http://example.com/1
找到链接: http://example.com/2
```

在这个示例中，我们解析了一个简单的 HTML 文档，找到所有的 `<a>` 元素并提取它们的 `href` 属性，有效地打印出文档中所有链接的 URL。`scraper` 库简化了 HTML 解析和使用 CSS 选择器选择特定元素的过程，使其成为 Rust 中进行网页抓取任务的首选。
