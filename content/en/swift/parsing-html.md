---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the process of accessing and extracting specific data from HTML data structures. Programmers parse HTML to manipulate web content, automate web interactions and extract relevant info for data analysis.

## How to:

To parse HTML, we'll use a common Swift package known as SwiftSoup. To illustrate, let's parse an example HTML string and extract the title:

```Swift
import SwiftSoup

let html = "<html><head><title>Swift HTML Parsing!</title></head><body></body></html>"
do {
    let document: Document = try SwiftSoup.parse(html)
    let title: Element = try document.title()
    print("Title: \(title)") 
} catch Exception.Error(_, let message) {
    print (message)
} catch {
   print("An error occurred")
}
```
Running this would output:
```
Title: Swift HTML Parsing!
```

## Deep Dive

In the past, Swift developers might have used NSHTMLTextDocumentType from NSAttributedString, but this was verbose and limited. SwiftSoup emerged as a Swift adaptation of the popular Java library Jsoup, providing a well-featured, easy to use HTML parsing package.

Alternatives to SwiftSoup include Kanna and Swift-HTML-Parser, but SwiftSoup generally comes out on top due to its flexibility and feature set. For instance, SwiftSoup supports CSS selector syntax, while others do not.

Understanding the implementation details of SwiftSoup or any HTML parser requires a grasp of data structures like Document Object Models (DOM). When SwiftSoup parses an HTML string, it creates a DOM - essentially a tree structure, mirroring the HTML tags and their hierarchy. You can then traverse and manipulate this DOM tree, just as you would in JavaScript.

## See Also

- [Official SwiftSoup GitHub](https://github.com/scinfu/SwiftSoup): For full library documentation and usage examples.
- [Mozilla Developer Guide to DOM](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Introduction): To understand the underlying data structure the parsers work with.