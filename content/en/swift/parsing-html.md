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

## Why 
Many websites, especially those built with older technologies, still use HTML as their main language. By learning how to parse HTML, you can easily extract useful information from these websites and use it in your own applications or scripts.

## How To 
To parse HTML in Swift, we can use a popular library called SwiftSoup. First, install the library by adding the following line to your `Podfile`:

```Swift
pod 'SwiftSoup'
```

Next, import the library in your code:

```Swift
import SwiftSoup
```

Now, let's say we want to extract the title and description of a webpage. We can use the `get` function from SwiftSoup to get the HTML of the webpage:

```Swift
guard let url = URL(string: "https://www.example.com") else { return }
do {
    let html = try String(contentsOf: url)
} catch {
    print("Error: \(error)")
}
```

Next, we will use the `parse` function from SwiftSoup to parse the HTML and create a document object:

```Swift
do {
    let doc: Document = try SwiftSoup.parse(html)
} catch {
    print("Error: \(error)")
}
```

Now, we can use the `select` function from SwiftSoup to select specific elements from the HTML. For example, to get the title of the webpage, we can use the following code:

```Swift
do {
    let titleElement: Element? = try doc.select("title").first()
    let title = try titleElement?.text()
    // Output: Example Domain
} catch {
    print("Error: \(error)")
}
```

We can also get the description of the webpage by selecting the `meta` element with the property `name=description`:

```Swift
do {
    let metaElement: Element? = try doc.select("meta[name=description]").first()
    let description = try metaElement?.attr("content")
    // Output: This is an example website.
} catch {
    print("Error: \(error)")
}
```

## Deep Dive
HTML stands for Hypertext Markup Language and is the standard language used for creating webpages. It is made up of various tags and attributes that define the content and structure of a webpage. By understanding these tags and attributes, you can better manipulate and extract information from HTML.

In SwiftSoup, the `select` function uses CSS selectors to select specific elements from the HTML document. CSS selectors are patterns used to select specific elements based on their tag name, class, id, or attributes. There are various types of CSS selectors, such as element selectors, class selectors, and attribute selectors, that can be used depending on your specific needs.

It is important to note that while parsing HTML can be useful, it is also important to respect the website's terms of use and not abuse the information extracted. Make sure to always check the website's terms and conditions before using any data obtained through parsing.

## See Also
- [SwiftSoup Github Page](https://github.com/scinfu/SwiftSoup)
- [HTML Parser in Swift: Parse, extract, modify and render HTML using SwiftSoup](https://medium.com/@ppoh71/html-parser-in-swift-parse-extract-modify-and-render-html-using-swiftsoup-c0ed4c2f31e8)
- [CSS Selectors Guide](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Selectors)