---
date: 2024-01-20 15:32:27.978327-07:00
description: 'How to: Kotlin makes parsing HTML straightforward with libraries like
  Jsoup. Here''s how you do it.'
lastmod: '2024-03-13T22:45:00.046640-06:00'
model: unknown
summary: Kotlin makes parsing HTML straightforward with libraries like Jsoup.
title: Parsing HTML
weight: 43
---

## How to:
Kotlin makes parsing HTML straightforward with libraries like Jsoup. Here's how you do it:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Sample Page</title></head><body><p>This is a test.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("Title: $title")  // Output: Title: Sample Page

    val pText = doc.select("p").first()?.text()
    println("Paragraph: $pText")  // Output: Paragraph: This is a test.
}
```

We grab the title and paragraph text, just scratching the surface of what Jsoup can do. But it's a start.

## Deep Dive:
Before Kotlin, Java was the go-to for this, often clumsily. Jsoup flipped the script by providing a jQuery-esque approach. Parsing HTML isn't exclusive to Jsoup though; other libraries like HtmlUnit or even regex (though advised against) exist. With Jsoup, you ensure that your parsing respects the document's structure. It uses a DOM model, enabling selection and manipulation of elements. It's resilient, too—it can parse even the messiest HTML.

## See Also:
Dive deeper into Jsoup:

- Jsoup official documentation: https://jsoup.org/
- "Kotlin for Android Developers" book: https://antonioleiva.com/kotlin-android-developers-book/
- Kotlin Programming Language official site: https://kotlinlang.org/

For broader discussions and tutorials on web scraping and parsing:

- Web Scraping with Kotlin and Jsoup: https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- Parsing HTML on Android with Kotlin and Jsoup: https://proandroiddev.com/parsing-html-on-android-1b766658be6a
