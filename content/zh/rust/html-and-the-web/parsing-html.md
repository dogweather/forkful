---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:57.137642-07:00
description: "\u5982\u4F55\u505A\uFF1A \u8981\u5728 Rust \u4E2D\u89E3\u6790 HTML\uFF0C\
  \u4F60\u901A\u5E38\u4F1A\u4F7F\u7528 `scraper` \u5E93\uFF0C\u5B83\u63D0\u4F9B\u4E86\
  \u4E00\u4E2A\u9AD8\u7EA7\u63A5\u53E3\u6765\u904D\u5386\u548C\u64CD\u4F5C HTML \u6587\
  \u6863\u3002 \u9996\u5148\uFF0C\u5C06 `scraper` \u6DFB\u52A0\u5230\u4F60\u7684 `Cargo.toml`\
  \ \u4E2D\uFF1A."
lastmod: '2024-04-05T21:53:47.839061-06:00'
model: gpt-4-0125-preview
summary: "\u9996\u5148\uFF0C\u5C06 `scraper` \u6DFB\u52A0\u5230\u4F60\u7684 `Cargo.toml`\
  \ \u4E2D\uFF1A."
title: "\u89E3\u6790HTML"
weight: 43
---

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
