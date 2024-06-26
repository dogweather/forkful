---
date: 2024-02-03 19:02:37.703546-07:00
description: "How to: To parse HTML in Rust, you'll often use the `scraper` crate,\
  \ which provides a high-level interface to traverse and manipulate HTML documents.\u2026"
lastmod: '2024-03-13T22:44:59.893584-06:00'
model: gpt-4-0125-preview
summary: To parse HTML in Rust, you'll often use the `scraper` crate, which provides
  a high-level interface to traverse and manipulate HTML documents.
title: Parsing HTML
weight: 43
---

## How to:
To parse HTML in Rust, you'll often use the `scraper` crate, which provides a high-level interface to traverse and manipulate HTML documents.

First, add `scraper` to your `Cargo.toml`:

```toml
[dependencies]
scraper = "0.12.0"
```

Next, here's a simple example that extracts all link URLs from a given HTML string:

```rust
extern crate scraper;

use scraper::{Html, Selector};

fn main() {
    let html = r#"
    <html>
    <body>
        <a href="http://example.com/1">Link 1</a>
        <a href="http://example.com/2">Link 2</a>
    </body>
    </html>
    "#;

    let document = Html::parse_document(html);
    let selector = Selector::parse("a").unwrap();

    for element in document.select(&selector) {
        let link = element.value().attr("href").unwrap();
        println!("Found link: {}", link);
    }
}
```

Output:

```
Found link: http://example.com/1
Found link: http://example.com/2
```

In this example, we parse a simple HTML document to find all `<a>` elements and extract their `href` attributes, effectively printing the URLs of all the links in the document. The `scraper` library simplifies HTML parsing and selecting specific elements using CSS selectors, making it a go-to for web scraping tasks in Rust.
