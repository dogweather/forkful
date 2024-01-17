---
title:                "Parsing html"
html_title:           "TypeScript recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML is the process of analyzing a string of HTML code in order to extract meaningful information from it. Programmers do this in order to automate tasks such as web scraping or data extraction, as well as to normalize and validate HTML for use in web development.

## How to:
### Using the built-in DOMParser:
```
TypeScript
const parser = new DOMParser();
const htmlString = '<html><body><p>Hello World!</p></body></html>';
const htmlDocument = parser.parseFromString(htmlString, 'text/html');
console.log(htmlDocument.documentElement.outerHTML);
```
Output: `<html><body><p>Hello World!</p></body></html>`

### Using third-party libraries:
```
TypeScript
import * as cheerio from 'cheerio';
const htmlString = '<html><body><p>Hello World!</p></body></html>';
const $ = cheerio.load(htmlString);
$('p').text();
```
Output: `Hello World!`

## Deep Dive:
Parsing HTML has been around since the early days of the internet, when browsers needed to interpret HTML and display it on the page. As web development advanced, so did the need for more advanced parsing techniques. While the examples above use the built-in DOMParser and the third-party library Cheerio, there are many other approaches to parsing HTML, such as Regular Expressions and custom parsers.

The DOMParser is a native JavaScript API, and therefore does not require any external dependencies. It is widely supported by all modern browsers and is the recommended approach for parsing HTML in TypeScript. Cheerio, on the other hand, is a third-party library that simulates a jQuery-like API for traversing and manipulating HTML. It is commonly used in web scraping and data extraction tasks.

When parsing HTML, it is important to understand the different types of nodes in the DOM tree, such as element nodes, text nodes, and comment nodes. It is also important to consider how to handle errors and invalid HTML. Proper handling of these issues will ensure the accuracy and efficiency of your parsing.

## See Also:
- [DOMParser API Documentation](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- [Cheerio Library Documentation](https://cheerio.js.org/)
- [Regular Expressions Tutorial](https://www.regular-expressions.info/tutorial.html)