---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:57.137642-07:00
description: "\u5728 Rust \u4E2D\u89E3\u6790 HTML \u610F\u5473\u7740\u4ECE HTML \u6587\
  \u6863\u4E2D\u62BD\u53D6\u6570\u636E\uFF0C\u8FD9\u5BF9\u4E8E\u7F51\u9875\u6293\u53D6\
  \u3001\u6570\u636E\u63D0\u53D6\u6216\u6784\u5EFA\u7F51\u9875\u722C\u866B\u81F3\u5173\
  \u91CD\u8981\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u81EA\u52A8\
  \u5316\u4ECE\u7F51\u7EDC\u6536\u96C6\u4FE1\u606F\u3001\u5206\u6790\u7F51\u7EDC\u5185\
  \u5BB9\u6216\u5C06\u5185\u5BB9\u4ECE\u4E00\u4E2A\u5E73\u53F0\u8FC1\u79FB\u5230\u53E6\
  \u4E00\u4E2A\u5E73\u53F0\u3002"
lastmod: '2024-03-13T22:44:47.518048-06:00'
model: gpt-4-0125-preview
summary: "\u5728 Rust \u4E2D\u89E3\u6790 HTML \u610F\u5473\u7740\u4ECE HTML \u6587\
  \u6863\u4E2D\u62BD\u53D6\u6570\u636E\uFF0C\u8FD9\u5BF9\u4E8E\u7F51\u9875\u6293\u53D6\
  \u3001\u6570\u636E\u63D0\u53D6\u6216\u6784\u5EFA\u7F51\u9875\u722C\u866B\u81F3\u5173\
  \u91CD\u8981\u3002\u7A0B\u5E8F\u5458\u8FD9\u6837\u505A\u662F\u4E3A\u4E86\u81EA\u52A8\
  \u5316\u4ECE\u7F51\u7EDC\u6536\u96C6\u4FE1\u606F\u3001\u5206\u6790\u7F51\u7EDC\u5185\
  \u5BB9\u6216\u5C06\u5185\u5BB9\u4ECE\u4E00\u4E2A\u5E73\u53F0\u8FC1\u79FB\u5230\u53E6\
  \u4E00\u4E2A\u5E73\u53F0\u3002."
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
