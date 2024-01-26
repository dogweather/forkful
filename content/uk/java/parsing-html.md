---
title:                "Парсинг HTML"
date:                  2024-01-20T15:32:11.518269-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/java/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Parsing HTML means extracting data from HTML files. Programmers do this to interact with web content, automate tasks, or scrape data.

## How to: (Як це зробити:)
Let's use Jsoup, a Java library for HTML parsing. Add it to your project with Maven:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.14.3</version>
</dependency>
```

Here's a snippet that fetches a webpage and grabs the title:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class HtmlParserDemo {
    public static void main(String[] args) throws Exception {
        Document doc = Jsoup.connect("http://example.com").get();
        String title = doc.title();
        System.out.println("Title: " + title);
    }
}
```

Sample output:

```
Title: Example Domain
```

## Deep Dive (Поглиблений Розгляд):
Historically, parsing HTML was complicated due to messy code and lack of standards. Tools like Jsoup revolutionized the process by providing a forgiving parser and a jQuery-like interface.

Alternatives to Jsoup include HtmlUnit for testing web apps, and XPath for queries on XML-like structures.

Implementation-wise, Jsoup uses a DOM (Document Object Model) approach, building an in-memory tree representation of the HTML. It can fix bad HTML on the fly, making it resilient to real-world web pages.

## See Also (Дивіться також):
- [Jsoup Official Documentation](https://jsoup.org/)
- [HtmlUnit](http://htmlunit.sourceforge.net/)
