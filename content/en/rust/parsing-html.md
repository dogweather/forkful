---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the process of checking and interpreting HTML formatted document. It's done to transform unstructured HTML data into a structured format, unlocking the potential to effectively utilize this data.

## How to:

To parse HTML in Rust, we use a crate called `scraper`. Here's a basic example:

```Rust
use scraper::{Html, Selector};

fn main() {
    let html = r#"<p class='foo'>Hello, world!</p>"#; 
    let fragment = Html::parse_fragment(&html);
    let selector = Selector::parse(".foo").unwrap();
    for element in fragment.select(&selector) {
        assert_eq!(element.inner_html(), "Hello, world!");
    }
}
```

This code will select the first HTML element with the “foo” class and print its inner HTML.

## Deep Dive

**Historical context:** The need for web scraping - including HTML parsing - blossomed with the growth of the internet. Rust's efficiency and safety features make it a suitable choice for this task.

**Alternatives:** There are quite a few crates available for HTML parsing in Rust like `html5ever`, `kuchiki`, `reqwest` etc. You can choose one as per your requirement.

**Implementation details:** In Rust, HTML parsing libraries work by tokenizing the HTML string, parsing it according to HTML rules, and then creating a Document Object Model (DOM) tree, upon which selection and manipulation operations can be performed effectively.

## See Also

1. **Rust Documentation:** https://doc.rust-lang.org/book/
2. **Rust HTML parsing libraries like Scraper:** https://docs.rs/scraper/0.12.0/scraper/
3. **Html5ever package:** https://github.com/servo/html5ever
4. **Kuchiki package:** https://github.com/kuchiki-rs/kuchiki
5. **Reqwest package:** https://github.com/seanmonstar/reqwest
6. **Web scraping with Rust tutorial:** https://www.programmersought.com/article/31025816377/