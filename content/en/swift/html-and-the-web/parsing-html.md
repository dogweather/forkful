---
date: 2024-02-03 19:02:45.429822-07:00
description: "How to: Swift, by default, doesn't include a built-in library for HTML\
  \ parsing, necessitating the use of third-party libraries to handle this task\u2026"
lastmod: '2024-03-13T22:45:00.394441-06:00'
model: gpt-4-0125-preview
summary: Swift, by default, doesn't include a built-in library for HTML parsing, necessitating
  the use of third-party libraries to handle this task effectively.
title: Parsing HTML
weight: 43
---

## How to:
Swift, by default, doesn't include a built-in library for HTML parsing, necessitating the use of third-party libraries to handle this task effectively. One of the most popular choices is SwiftSoup, a pure Swift library that offers jQuery-like syntax for HTML parsing and manipulation.

### Installation
First, you need to add SwiftSoup to your project. If you're using Swift Package Manager, you can add it to your `Package.swift` dependencies:

```swift
dependencies: [
    .package(url: "https://github.com/scinfu/SwiftSoup.git", from: "2.3.2")
]
```

### Example: Extracting Links from HTML
Suppose you have an HTML document and you want to extract all links (`<a href="...">`). With SwiftSoup, you can accomplish this easily:

```swift
import SwiftSoup

let html = """
<!DOCTYPE html>
<html>
<head>
    <title>Sample Page</title>
</head>
<body>
    <p>Welcome to our website</p>
    <a href="https://example.com/page1">Page 1</a>
    <a href="https://example.com/page2">Page 2</a>
</body>
</html>
"""

do {
    let doc: Document = try SwiftSoup.parse(html)
    let links: Elements = try doc.select("a")
    for link in links.array() {
        let linkHref: String = try link.attr("href")
        let linkText: String = try link.text()
        print("\(linkText) - \(linkHref)")
    }
} catch Exception.Error(let type, let message) {
    print("Error type: \(type) Message: \(message)")
} catch {
    print("error")
}
```

### Sample Output
The previous code extracts URLs and their text from the HTML, outputting:

```
Page 1 - https://example.com/page1
Page 2 - https://example.com/page2
```

This basic example demonstrates how to leverage SwiftSoup for parsing HTML documents. By exploring SwiftSoup's documentation further, you can find numerous methods to navigate, search, and modify the HTML content, empowering your Swift applications to process complex web content with ease.
