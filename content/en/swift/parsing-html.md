---
title:                "Parsing html"
html_title:           "Swift recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the act of analyzing a webpage's source code and extracting specific information from it. Programmers often do this in order to scrape data from websites or to manipulate the webpage's content programmatically.

## How to:

To parse HTML in Swift, we can use a library called "SwiftSoup." First, we need to import the library into our project. Then, we can create a `Document` object by passing in the URL of the webpage we want to parse. For example:

```Swift
import SwiftSoup

do {
  let url = "https://www.example.com"
  let doc = try SwiftSoup.parse(url)
} catch Exception {
  print("Error: Could not parse webpage")
}
```

Once we have the `Document` object, we can extract specific elements or data from it using CSS selectors. For example, if we want to get all the links on the webpage, we can use the `select` function and pass in the CSS selector for links, "a." This will return a `Elements` object, which we can then loop through to get each individual link. Here's how it looks:

```Swift
let links = try doc.select("a")
for link in links {
  print(link.attr("href"))
}
```

## Deep Dive:

Parsing HTML has been a common practice since the early days of the internet. In fact, it was one of the main ways to scrap or extract data from webpages before API's became widely available. Alternatives to parsing HTML include using API's or web scraping services, but those may not always be feasible or available.

SwiftSoup is built on top of the Java library Jsoup, which is a popular and well-maintained HTML parser. Implementing parsing HTML in Swift allows us to easily integrate it into our iOS or macOS projects.

## See Also:

- [SwiftSoup GitHub repository](https://github.com/scinfu/SwiftSoup)
- [Jsoup website](https://jsoup.org/)