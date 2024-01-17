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

## What & Why?

Parsing HTML is the process of analyzing and extracting data from HTML code. Programmers use this method to gather and manipulate information from web pages. It is an essential technique for data scraping, web crawling, and web scraping.

## How to:

```Kotlin 
val doc = Jsoup.connect("https://www.example.com").get()
val title = doc.title() 
println("The website title is: $title")
```

The above code uses the Jsoup library to connect to a website and retrieve the HTML code. It then uses the "title()" function to extract the title tag from the code, which represents the webpage's title. Finally, the program prints out the title.

## Deep Dive:

Parsing HTML has been around since the early days of the internet, with the first HTML parser being developed in 1993. It has since become a widespread practice among programmers due to the growth of web technologies and the increasing demand for data-driven applications.

While HTML parsing can be done manually, using tools and libraries like Jsoup makes the process much more efficient and reliable. Other alternatives to HTML parsing include using Regular Expressions or building custom parsers from scratch, but these can be more time-consuming and error-prone compared to using specialized libraries.

Under the hood, HTML parsing involves converting the HTML code into a structured document object model (DOM) and then using this model to extract the desired data. It follows a set of rules defined by the HTML specification to ensure that the extracted data is accurate and in the correct format.

## See Also:

- [W3C HTML5 Specification](https://www.w3.org/TR/2017/REC-html52-20171214/)
- [Jsoup Library Documentation](https://jsoup.org/apidocs/)
- [Tutorial: How to Parse HTML in Kotlin](https://www.baeldung.com/kotlin-html-parsing)