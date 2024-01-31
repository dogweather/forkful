---
title:                "Parsing HTML"
date:                  2024-01-20T15:32:16.739950-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML means digging through the markup to extract data like text, links, or other elements. We do it to interact with or scrape web content, automate browsing tasks, or test web apps.

## How to:

Let's use Jsoup, a handy library for working with real-world HTML. First, add the dependency:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

Now to the fun part. Here's how to grab a webpage's title and print it:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParser {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String title = doc.title();
        System.out.println("Title: " + title);
    }
}
```

Output:

```
Title: Example Domain
```

How about extracting all the links?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... inside the main or another method
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("Link: " + link.attr("href"));
}
```

## Deep Dive

Once upon a time, HTML was tamed by regex patterns, a method both error-prone and nightmarish for complex documents. Enter Jsoup in the late aughts, providing a jQuery-like interface for Java to parse, traverse, and manipulate HTML.

Jsoup is not the only choice. There's HtmlUnit for full-fledged web app testing with JavaScript support, but it's heavier and more complex. For lightweight tasks, Apache Commons Validator is great just for extracting URLs.

Under the hood, Jsoup uses a DOM parser, which models the entire document in memory as a tree. This approach makes selecting and navigating the HTML structure a breeze. Plus, it's forgiving with sloppy HTML, fixing issues on-the-fly to ensure robust parsing.

Remember, when scraping, always check a site's `robots.txt` and terms of service to avoid legal troubles or getting IP-banned.

## See Also

- Jsoup Official Documentation: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
