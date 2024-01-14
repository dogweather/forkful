---
title:                "PHP recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/php/parsing-html.md"
---

{{< edit_this_page >}}

## Why
HTML is the language that powers the web. It is the backbone of every website and contains all the information needed to display a webpage. As a programmer, being able to parse HTML is a valuable skill as it allows you to extract and manipulate data from websites. This can be useful for automating tasks, data scraping, and building web applications.

## How To
```PHP
// Create a new DOMDocument object
$document = new DOMDocument();

// Load HTML from a URL
$document->loadHTMLFile("https://www.example.com");

// Create a new DOMXPath object
$xpath = new DOMXPath($document);

// Specify the HTML element to extract
$element = "//h1";

// Get the value of the first match of the element
echo $xpath->query($element)->item(0)->nodeValue;

// Output: Example Domain
```

To parse HTML in PHP, you will need to use the built-in DOMDocument and DOMXPath classes. These classes allow you to load an HTML file or string and then use XPath expressions to select specific elements. In the example above, we are using the DOMXPath object to query for the first h1 element on the webpage and printing out its value.

## Deep Dive
XPath stands for XML Path Language and is a query language used for traversing XML documents. HTML is a type of XML document, which is why we can use XPath to navigate through it. XPath expressions, such as the one used in our code example, allow us to select specific nodes in the HTML tree structure.

The DOMDocument and DOMXPath classes are part of the DOM extension in PHP. The DOM extension also provides methods for manipulating HTML elements, such as adding or removing nodes, and modifying attributes.

HTML parsing can sometimes be tricky because not all websites follow the same structure and formatting. It is important to familiarize yourself with XPath and HTML in order to effectively extract the data you need.

## See Also
- [PHP Documentation on DOM](https://www.php.net/manual/en/book.dom.php)
- [W3Schools XPath Tutorial](https://www.w3schools.com/xml/xpath_intro.asp)
- [How to Parse HTML in PHP](https://www.phpzag.com/parsing-html-using-php/)