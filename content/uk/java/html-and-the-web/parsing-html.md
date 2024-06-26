---
date: 2024-01-20 15:32:11.518269-07:00
description: "How to: (\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\
  \u0438:) Let's use Jsoup, a Java library for HTML parsing. Add it to your project\
  \ with Maven."
lastmod: '2024-04-05T21:53:49.291569-06:00'
model: unknown
summary: "(\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438:)\
  \ Let's use Jsoup, a Java library for HTML parsing."
title: "\u041F\u0430\u0440\u0441\u0438\u043D\u0433 HTML"
weight: 43
---

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
