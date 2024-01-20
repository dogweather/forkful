---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a webpage is the process of retrieving and storing HTML data from a URL. Programmers often do this to scrape data, test website functionality, or analyze a site's structure.

## How to:

In Kotlin, you can use `java.net.URL` and `BufferedReader` to download a web page. Here's how:

```Kotlin
import java.net.URL
import java.io.BufferedReader

fun downloadWebPage(url: String): String {
    return URL(url).openStream().bufferedReader().use(BufferedReader::readText)
}

fun main() {
    val content = downloadWebPage("https://www.example.com")
    println(content)
}
```

This script opens a URL stream, buffers the input, and reads all text content. Running it would print the HTML of "https://www.example.com" to your console.

## Deep Dive

Web page downloading dates back to the WWW's early days when most pages were simple HTML documents. In modern times, with dynamic content and complex scripts, handling just HTML may not suffice.

Alternatives include libraries like Jsoup, which is great for parsing HTML, or Selenium, better suited for pages with dynamic content.

It's crucial to understand downloading a web page retrieves a static snapshot, not live data. Any subsequent changes on the page won't reflect in the downloaded data.

## See Also:

- Kotlin documentation, specifically reading data from a URL: https://kotlinlang.org/docs/tutorials/kotlin-for-py/networking.html
- Tutorial on using Jsoup with Kotlin: https://www.baeldung.com/kotlin-jsoup
- Selenium’s GitHub page explaining its use with Kotlin: https://github.com/SeleniumHQ/selenium/tree/trunk/java/kotlin/src/org/openqa/selenium.