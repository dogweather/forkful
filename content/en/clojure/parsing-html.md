---
title:                "Parsing html"
html_title:           "Clojure recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
HTML parsing is the process of extracting data from HTML documents, which are the foundation of web pages. Programmers do this to retrieve specific information from websites, such as prices of products or titles of articles. This data can then be used for various purposes, such as building web crawlers or creating data visualizations.

## How to:
Parsing HTML in Clojure is made easy with the help of the library "Enlive". First, we need to add the library as a dependency in our project.clj file:

```Clojure
:dependencies [[net.cgrand/enlive "1.1.6"]]
```

Next, we can use Enlive's "html-resource" function to retrieve the HTML document from a URL or a local file. For example, if we want to parse the HTML from the webpage "https://example.com":

```Clojure
(require '[net.cgrand.enlive-html :as html])

(html/html-resource "https://example.com")
```

This will return a data structure that we can navigate using Enlive's selectors. For example, if we want to extract the title of the webpage, we can use the "html/select" function and provide a CSS selector for the title element:

```Clojure
(html/select (html/html-resource "https://example.com") 
             [:head :title])

;; Output: ["Example Domain"]
```

## Deep Dive:
Using Enlive for HTML parsing in Clojure has become the preferred method due to its simplicity and powerful features. Before Enlive, the main alternative was using Java DOM parsers, which required more code and knowledge of Java. Enlive also provides a selector syntax that is similar to CSS, making it easier for web developers to understand and use.

Under the hood, Enlive works by transforming the HTML document into a data structure called "selectors", which can then be used to navigate and extract data from the document. This approach is known as "structure-based" parsing and is different from the traditional "text-based" parsing.

## See Also:
- Enlive documentation: https://github.com/cgrand/enlive/blob/master/README.md
- HTML parsing alternatives for Clojure: https://github.com/functional-koans/clojure-koan-answers/wiki/Alternate-HTML-libraries-for-Clojure