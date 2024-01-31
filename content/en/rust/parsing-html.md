---
title:                "Parsing HTML"
date:                  2024-01-20T15:33:53.407056-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the act of taking a string of HTML and breaking it down into a data structure your program can understand and manipulate. Programmers do this to interact with web content, extract information, and automate web-related tasks.

## How to:

To parse HTML in Rust, you'll likely want to use a crate like `scraper` or `select`. Here's a quick example using `scraper`:

```Rust
use scraper::{Html, Selector};

fn main() {
    // The HTML input as a string
    let html = r#"
        <html>
            <body>
                <p>Hello, world!</p>
            </body>
        </html>
    "#;

    // Parse the HTML string
    let document = Html::parse_document(html);
    
    // Create a selector to find all <p> tags
    let selector = Selector::parse("p").unwrap();

    // Iterate over elements matching the selector
    for element in document.select(&selector) {
        // Print the text inside each <p> tag
        println!("{}", element.text().collect::<Vec<_>>().concat());
    }
}
```

Output:
```
Hello, world!
```

## Deep Dive

Way back, parsing HTML was a messy affair. Libraries varied, standards were a moving target, and languages differed in their approaches. Today, Rust's ecosystem offers sturdy crates for parsing, like `scraper` which leans on `html5ever` and `selectors` libraries. `html5ever` is particularly interesting; it's based on the HTML parsing algorithm specified by the WHATWG, making it on par with how modern browsers parse HTML.

Alternatives to `scraper` include `select`, which offers similar functionality but different ergonomics. Low-level parsing is possible with `html5ever` itself if you need more control.

Often, parsing HTML is part of web scraping, where you extract data from websites. It's important (and ethical) to respect a site's `robots.txt` and terms of service when scraping.

Implementation-wise, always remember parsing is just the starting point. Sanitization and validation are key to avoiding security issues like XSS (Cross-Site Scripting) attacks, especially if you plan to display or store parsed data.

## See Also

- The `scraper` crate: https://crates.io/crates/scraper
- The `select` crate: https://crates.io/crates/select
- The `html5ever` GitHub repo: https://github.com/servo/html5ever
- The Rust Cookbook's "Web scraping" section: https://rust-lang-nursery.github.io/rust-cookbook/web/scraping.html
- WHATWG HTML parsing spec: https://html.spec.whatwg.org/multipage/parsing.html
- Rust's guide on error handling: https://doc.rust-lang.org/book/ch09-00-error-handling.html (to deal with potential `unwrap` panics)
