---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML means dissecting text written in the HTML language into a structured format that your code can understand. We do this to extract the necessary parts of HTML code and use it in our applications.

## How to:
In Kotlin, we can use a library like Jsoup, which makes parsing simple. Here's a piece of code that downloads and parses a web page.

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val doc = Jsoup.connect("http://example.com").get()
    val title = doc.title()
    println("Website title: $title")
}
```

Running this code will print the title of the web page:

```  
Website title: Example Domain
```

## Deep Dive
HTML parsing and Kotlin have evolved independently over the years. Earlier, parsing used to be a tedious process involving writing XML or DOM parsers. SDKs now provide libraries like Jsoup which simplifies parsing and manipulation of HTML.

An alternative to parsing HTML is to use APIs if available. However, in the absence of APIs or the need for web scraping, HTML parsing becomes necessary.

The Jsoup library works by reading the HTML into memory, understanding its hierarchy, and providing interfaces to access and modify the DOM. It is also capable of handling ill-formed HTML gracefully.

## See Also
Check out these sources for more information:

- [Jsoup Documentation](https://jsoup.org/)
- [Kotlin Documentation](https://kotlinlang.org/docs/home.html)
- [HTML Parsing and Web Scraping in Java Using Jsoup](https://www.baeldung.com/java-with-jsoup)