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

## Why

If you've ever wanted to extract specific information from a website or automate web scraping, parsing HTML can be a valuable skill to have in your programming tool belt. With Rust, you can easily create efficient and reliable HTML parsers to make your life easier.

## How To

Parsing HTML involves extracting information from a string of structured text, which can be broken down into smaller, more manageable pieces. In Rust, we can use the `html5ever` library to access the HTML parser and `reqwest` for making HTTP requests. Let's first set up our dependencies in the `Cargo.toml` file:

```
[dependencies]
html5ever = "0.25"
reqwest = "0.11"
```

Next, we can create a `main.rs` file and import our dependencies:

```
use html5ever::parse_document;
use html5ever::rcdom::RcDom;
use reqwest::blocking::get;
```

Then, we can make a request to a website and retrieve the HTML content:

```
let response = get("https://www.example.com").expect("Unable to make request");
let body = response.text().expect("Unable to read response body");
```

Using the `parse_document` function, we can create a DOM representation of the HTML document:

```
let dom = parse_document(RcDom::default(), Default::default())
    .from_utf8()
    .read_from(&mut body.as_bytes());
```

Now, we can use methods and iterators to navigate the DOM and extract the information we need. For example, to get all the links from the webpage, we can use the `descendants` method to get all elements in the DOM and filter for links only:

```
let links: Vec<String> = dom.document.children[0].descendants()
    .filter_map(|node| {
        if let Some(element) = node.as_element() {
            if element.name.local == "a" {
                return element.get_attribute("href").map(|link| link.to_string());
            }
        }
        return None;
    }).collect();
```

This will give us a vector of strings, each containing a link from the webpage. We can then process this data further or save it to a file for later use.

## Deep Dive

The `html5ever` library follows the HTML5 parsing algorithm and provides a simple and efficient way to parse HTML using a DOM (Document Object Model) representation. You can also use the `html5ever` crate to handle more complex HTML parsing scenarios, such as handling errors and creating custom attributes.

One of the key features of `html5ever` is the ability to handle invalid HTML and output a valid DOM, making it suitable for real-world situations where website code may not always be perfect. It also provides the ability to sanitize and clean up HTML, making it even more versatile for various parsing needs.

## See Also

- [Rust Docs for html5ever](https://docs.rs/html5ever/0.25.0/html5ever/)
- [Rust Docs for reqwest](https://docs.rs/reqwest/0.11.0/reqwest/)
- [Official Rust Website](https://www.rust-lang.org/)