---
title:                "Parsing html"
html_title:           "Java recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the process of extracting data from HTML code, which is the standard markup language used for creating web pages. Programmers often need to parse HTML in order to extract relevant information from websites for various purposes, such as data analysis or web scraping.

## How to: 

To parse HTML in Java, we can use the JSoup library, which provides a simple and flexible API for working with HTML. Here's an example of how to use JSoup to extract the text from a paragraph element in an HTML document:

```Java
// import the JSoup library
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;

// parse the HTML document
Document doc = Jsoup.parse("<p>Hello, world!</p>");

// get the paragraph element and extract the text
Element paragraph = doc.select("p").first();
String text = paragraph.text();

// print the extracted text
System.out.println(text);
```

The output of this code would be: `Hello, world!`

## Deep Dive

HTML parsing has been a common task for web developers since the early days of the internet. Before the introduction of libraries like JSoup, developers had to write their own parsing code or rely on complex and often unreliable regular expressions. JSoup, first released in 2009, revolutionized HTML parsing in Java by providing a straightforward and easy-to-use API.

While JSoup is the go-to library for HTML parsing in Java, there are other alternatives available, such as HTMLCleaner and NekoHTML. However, JSoup is still the most popular choice due to its intuitive API and efficient performance.

Under the hood, JSoup uses a robust parsing algorithm to analyze and transform the HTML code into a Document object, which can then be traversed and manipulated using CSS selectors or DOM methods. This enables programmers to extract specific data elements from HTML documents with ease.

## See Also

To get started with JSoup, check out the official documentation at https://jsoup.org/. For more information on web scraping and data extraction, you can also refer to resources such as the Beautiful Soup library for Python or the Selenium framework for automated testing.