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

## Why

Parsing HTML is a common task in web development. It involves extracting data from HTML documents and using it for various purposes such as web scraping, data analysis, and content extraction. Clojure provides powerful tools for parsing HTML, making it a great language choice for this task.

## How To

To get started with parsing HTML in Clojure, we will use the `cljs-beautifulsoup` library. It provides a convenient interface for parsing and navigating HTML documents.

First, we need to import the library into our project using the `ns` macro:

```Clojure
(ns my-project.core
  (:require [cljs-beautifulsoup.core :as soup]))
```

Next, we can use the `parse` function to load an HTML document from a URL or a file:

```Clojure
(def doc (soup/parse "http://www.example.com"))
```

We can then use various functions such as `select` and `find` to navigate the document and extract data from it:

```Clojure
(soup/text (soup/select doc "h1")) ;; returns the text within the first h1 tag
(soup/attr (soup/find doc :a {:class "button"}) :href) ;; returns the value of the "href" attribute of the first link with class "button"
```

We can also use CSS selectors to easily target specific elements in the document:

```Clojure
(soup/html (soup/select doc "#content p")) ;; returns the HTML code within all <p> tags inside the element with id "content"
```

For more examples and details on the functions provided by `cljs-beautifulsoup`, check out its documentation.

## Deep Dive

Behind the scenes, `cljs-beautifulsoup` uses the popular `jsoup` library which is built on top of Java's `jsoup` library. This means that it has access to all the features and optimizations of the Java library, while providing a more idiomatic and functional interface in Clojure.

The library also supports advanced features such as manipulating HTML elements and attributes, handling invalid HTML, and handling character encoding, making it a robust choice for parsing HTML documents.

## See Also

- [cljs-beautifulsoup documentation](https://github.com/Jarzka/cljs-beautifulsoup)
- [jsoup documentation](https://jsoup.org/cookbook/)
- [Official Clojure website](https://clojure.org/)