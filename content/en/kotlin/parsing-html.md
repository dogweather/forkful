---
title:                "Parsing html"
html_title:           "Kotlin recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Why
Parsing HTML, or extracting meaningful data from HTML documents, is a useful skill to have in today's digital age. Whether you are a web developer, data scientist, or simply someone trying to scrape information from a website, knowing how to parse HTML can come in handy. 

## How To
To start parsing HTML in Kotlin, we first need to import the necessary libraries. In this case, we will be using the popular Jsoup library for its simplicity and convenience.

```Kotlin
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
```

Next, we will need to fetch the HTML document we want to parse. This can be done using the `Jsoup.connect()` method, passing in the URL of the webpage as a parameter. We can also specify a timeout period for the connection in milliseconds.

```Kotlin
val doc: Document = Jsoup.connect("https://example.com")
                .timeout(5000)
                .get()
```

Now that we have the HTML document, we can start extracting the data we want. Jsoup provides various methods for selecting specific elements based on attributes, classes, or even CSS selectors.

```Kotlin
// Selecting all links on the webpage and printing their text and URLs
val links = doc.select("a")
for (link in links) {
    val linkText = link.text()
    val linkUrl = link.attr("abs:href")
    println("$linkText - $linkUrl")
}
```

We can also use Jsoup's built-in methods to extract specific elements or information from the HTML document.

```Kotlin
// Extracting the page title and meta description
val pageTitle = doc.title()
val metaDescription = doc.select("meta[name=description]")
                        .attr("content")
```

The possibilities are endless when it comes to parsing HTML using Kotlin and Jsoup. With a little practice, you'll be able to extract any data you need from any webpage.

## Deep Dive
Under the hood, Jsoup uses a combination of DOM parsing and CSS selector to navigate and select elements from HTML documents. It emulates a web browser's DOM tree, making it easy to manipulate and extract data from the HTML.

One benefit of using Jsoup over other parsing libraries is its flexibility in handling malformed HTML. It can still extract data from poorly written HTML, making it perfect for web scraping projects.

## See Also
- [Jsoup Documentation](https://jsoup.org/cookbook/extracting-data/dom-navigation)
- [Kotlin Language Reference](https://kotlinlang.org/docs/reference/)