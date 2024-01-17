---
title:                "Parsing html"
html_title:           "Rust recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/rust/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the process of analyzing the structure of an HTML document and extracting the meaningful information from it. It is an essential task for web developers as HTML is the standard markup language used to create web pages. By parsing HTML, programmers can easily access and manipulate the content of a web page, making it an important skill for creating dynamic and interactive websites.

## How to:

To parse HTML in Rust, we can use a library called "scraper". First, let's add it as a dependency in our `Cargo.toml` file:

```Rust
[dependencies]
scraper = "0.12.0"
```

Next, we need to import the library in our code:

```Rust
use scraper::{Html, Selector};
```

Now we can use the `Html` struct to load our HTML document and the `Selector` struct to specify which elements we want to extract from the document. We can then use the `.select()` function to get a list of all elements that match our selector:

```Rust
let document = Html::parse_document(html_string); // html_string is a string containing our HTML document
let selector = Selector::parse("h1").unwrap(); // select all h1 elements
let h1_elements = document.select(&selector);
```

We can then iterate through the list and extract the desired information from each element:

```Rust
for h1 in h1_elements {
    let text = h1.text().collect::<Vec<_>>().join("");
    println!("{}", text); // print out the text content of each h1 element
}
```

This is a basic example, but you can use similar techniques to extract different types of data from an HTML document. For more information and a detailed tutorial, check out the "scraper" documentation.

## Deep Dive:

Parsing HTML has been a crucial aspect of web development since the beginning. In the early days, developers had to manually parse HTML using string manipulation techniques, which was error-prone and time-consuming. With the rise of programming languages and libraries like Rust and "scraper", this task has become much easier and more efficient.

Aside from "scraper", there are other alternatives for parsing HTML in Rust such as "html5ever" and "kuchiki". Each of these libraries has its own strengths and may be better suited for certain tasks. It's worth exploring and experimenting with different options to find the best fit for your needs.

Under the hood, "scraper" uses the "cssparser" library, which implements the CSS selector syntax. This allows developers to use familiar CSS selectors to specify which elements they want to extract from the HTML document.

## See Also:

- [scraper documentation](https://docs.rs/scraper/0.12.0/scraper/)
- [html5ever library](https://crates.io/crates/html5ever)
- [kuchiki library](https://crates.io/crates/kuchiki)
- [cssparser library](https://crates.io/crates/cssparser)