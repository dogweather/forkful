---
title:                "Parsing html"
html_title:           "C++ recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the process of analyzing and extracting data from HTML documents. It is especially useful for web developers who need to programmatically extract specific information from a website's source code. By parsing HTML, programmers can automate tasks such as data scraping, web crawling, and web page manipulation.

## How to:

To parse HTML in C++, we will use a popular library called libxml2. This library allows us to easily read and manipulate HTML documents. Below is a basic example of how to use libxml2 to parse HTML:

```C++
#include <libxml/HTMLparser.h>
#include <libxml/xpath.h>

// read the HTML document into a DOM tree
htmlDocPtr html_doc = htmlReadFile("example.html", NULL, HTML_PARSE_NOERROR | HTML_PARSE_NOWARNING);

// create an XPath evaluation context
xmlXPathContextPtr xpath_ctx = xmlXPathNewContext(html_doc);

// evaluate an XPath expression to find all <a> tags
xmlXPathObjectPtr xpath_obj = xmlXPathEvalExpression("//a", xpath_ctx);

// loop through the results and print out the text of each <a> tag
for (int i = 0; i < xpath_obj->nodesetval->nodeNr; i++) {
    xmlNodePtr node = xpath_obj->nodesetval->nodeTab[i];
    cout << xmlNodeGetContent(node) << endl;
}

// free the memory used by the XPath objects
xmlXPathFreeObject(xpath_obj);
xmlXPathFreeContext(xpath_ctx);

// free the DOM tree
xmlFreeDoc(html_doc);
```

The above code will print out the text of all <a> tags in the HTML document. You can also use libxml2 to modify the HTML document, such as adding or removing elements.

## Deep Dive

Parsing HTML has been a critical part of web development since the early days of the internet. Before the widespread use of libraries like libxml2, developers had to manually parse HTML using regular expressions, which was a tedious and error-prone process. Parsing HTML has also become increasingly important in the era of web scraping and data analysis, as it allows programmers to easily access and extract data from web pages.

While libxml2 is a popular choice for parsing HTML in C++, there are other libraries and tools available such as Boost.PropertyTree and the C++ HTML Parser. These alternatives may offer different features and performance, so it's worth exploring them to find the best fit for your project.

In terms of implementation details, libxml2 uses SAX (Simple API for XML) parsing, which reads through the HTML document in a linear manner, making it efficient for large documents. It also has integrated error handling and support for XPath expressions, making it a robust choice for parsing HTML.

## See Also

- [libxml2 documentation](http://xmlsoft.org/html/libxml-HTMLparser.html)
- [Boost.PropertyTree](https://www.boost.org/doc/libs/1_76_0/doc/html/property_tree.html)
- [C++ HTML Parser](https://github.com/lexborisov/HTMLParser)