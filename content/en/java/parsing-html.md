---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the process of analyzing a document to extract meaningful information. Programmers often parse HTML files to manipulate, extract, and transform the contained data, particularly in web scraping or data mining tasks.

## How to:

Let’s dive into some coding. We'll use the Jsoup, a popular library in Java. 

First, add the following Maven dependency to your project's pom.xml:

```Java
<dependency>
   <groupId>org.jsoup</groupId>
   <artifactId>jsoup</artifactId>
   <version>1.13.1</version>
</dependency>
```

Then, here's how to connect to a website and parse its HTML:

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
	
public class Main
{
    public static void main(String[] args) throws Exception
    {
        Document document = Jsoup.connect("http://example.com").get();
        System.out.println(document.title());
    }
}
```

Output:
```
Example Domain
```

Simple, isn't it? In the above example, the `Jsoup.connect().get()` opens a connection to the given URL and fetches the HTML. `document.title()` prints out the title of the webpage.

## Deep Dive

HTML parsing in Java goes way back and currently, libraries like Jsoup, HTMLUnit enrich the Java ecosystem. HTML parsers were not cool initially; their importance grew after the web scraping boom. 

Jsoup, for instance, provides APIs to extract and manipulate the data using DOM, CSS, and JQuery-like methods. 

However, the alternative parser, HTMLUnit, while a bit unwieldy, provides more than just parsing; it’s a complete web browser simulator. 

Fundamentally, parsing HTML involves breaking down HTML documents into parse trees. These trees can then be navigated and manipulated easily for data extraction. 

While using libraries like Jsoup simplifies our task, it’s crucial to understand the underlying concepts and the complexities involved in their creation. Cold hard fact: Underneath, it’s a web of tokens, syntax analysis, and tree construction - a complex mechanism.

## See Also

Here is some food for thought:

[Jsoup official documentation][1]: Need more insights into Jsoup features? 

[HTMLUnit official documentation][2]: Want to go beyond just parsing? Try HTMLUnit.

[HTML Parsing theory][3]: Interested in the mechanism of HTML parsing? Dive in. 

[1]: http://jsoup.org/
[2]: http://htmlunit.sourceforge.net/
[3]: https://html.spec.whatwg.org/multipage/parsing.html