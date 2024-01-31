---
title:                "Parsing HTML"
date:                  2024-01-20T15:34:11.799596-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/swift/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML means sifting through the soup of a website's code to find useful nuggets — text, links, images, etc. Programmers do it to extract data, automate web interactions, or import content into their apps.

## How to:

Swift doesn't have built-in HTML parsing; we need a helper. Let's use SwiftSoup, a Swift library reminiscent of Python's BeautifulSoup. First, add SwiftSoup to your project using Swift Package Manager.

Here's how it's done:

```Swift
import SwiftSoup

do {
    let html = "<html><head><title>First parse</title></head>"
                + "<body><p>Parsed HTML into a doc.</p></body></html>"
    let doc = try SwiftSoup.parse(html)
    let title = try doc.title()
    let bodyText = try doc.body()?.text()
    
    print(title) // Output: First parse
    print(bodyText) // Output: Parsed HTML into a doc.
} catch Exception.Error(let type, let message) {
    print("An error of type: \(type) occurred: \(message)")
} catch {
    print("An unknown error occurred")
}
```

## Deep Dive

HTML, or HyperText Markup Language, has been web's backbone since Tim Berners-Lee introduced it (and the web) in 1991. As web evolved, so has HTML, escalating the parsing complexity.

Here's why SwiftSoup shines:
- **User-Friendly**: Its API mirrors JQuery, meaning it's intuitive for those familiar with web dev.
- **Robustness**: Handles real-world HTML quirks well.
- **Performance**: Swift's fast, which matters for big parsing jobs.

Alternatives? Certainly!
- **WebKit**: Use this for heavier tasks like rendering web pages or executing JavaScript.
- **libxml2**: Hardcore C route, but you better be up for the challenge.
- **Regex**: Just no. It’s not a parser. Don't try to “parse” HTML with regex. Seriously.

However, remember that a parser like SwiftSoup doesn't just read the page as-is; it's oblivious to any content dynamically loaded by JavaScript. For that, head towards solutions involving WebKit or browser headless modes.

## See Also

- SwiftSoup on GitHub: [https://github.com/scinfu/SwiftSoup](https://github.com/scinfu/SwiftSoup)
- Swift Package Manager: [https://swift.org/package-manager/](https://swift.org/package-manager/)
- WebKit documentation: [https://developer.apple.com/documentation/webkit](https://developer.apple.com/documentation/webkit)
- Handling dynamic content: [Selenium WebDriver](https://www.selenium.dev/documentation/en/) (not Swift-specific but relevant for automated interactions with dynamic web pages)
