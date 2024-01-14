---
title:                "Clojure recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to extract specific information from a website, but didn't want to waste time manually searching through HTML? Parsing HTML is a useful skill to have when working with webpage data and can save you time and effort.

## How To

To parse HTML in Clojure, we'll be using the Enlive library. You can import it into your project by adding the following dependency to your `project.clj` file:

```clojure
[net.cgrand/enlive "1.1.6"]
```

First, we'll need to load in the HTML content from a website. We can do this by using the `html-resource` function and passing in the URL of the webpage we want to parse. Here's an example of how we can do this:

```clojure
(ns demo.core
  (:require [net.cgrand.enlive-html :refer :all]
            [net.cgrand.io.file :as io]))

(let [html (html-resource "http://example.com")
      content (io/input-stream->bytes html)]
  (println content))
```

In the code above, we're using the `html-resource` function to load the HTML content from the webpage into a variable called `html`. Then, using the `io/input-stream->bytes` function, we're reading the HTML content and storing it in the `content` variable. Finally, we're printing out the HTML content using `println`.

Next, we'll need to target specific elements within the HTML document. Enlive provides a `select` function that allows us to use CSS selectors to target elements. Here's an example of how we can select all the links on a webpage:

```clojure
(ns demo.core
  (:require [net.cgrand.enlive-html :refer :all]
            [net.cgrand.io.file :as io]))

(defn parse-links [html]
  (select html [:a]))

(let [html (html-resource "http://example.com")
      links (parse-links html)]
  (println links))
```

In the code above, we've defined a function called `parse-links` that takes in the HTML content and uses `select` with the `:a` selector to target all the `<a>` tags on the webpage. We then pass the `html` variable into the function and store the returned links in a variable called `links`. Finally, we print out the `links` variable to see the results.

## Deep Dive

While the examples above only scratch the surface of parsing HTML in Clojure, there are many more useful functions and selectors available in Enlive. You can also use the `content` function to extract the text content of an element, the `attr` function to get a specific attribute from an element, and the `html-resource` function can also take in a local file path instead of a URL.

Additionally, there are other libraries available that offer more advanced HTML parsing capabilities, such as Clj-tagsoup and Clj-htmlparser. These libraries may be better suited for more complex HTML documents.

## See Also

- [Enlive Documentation](https://github.com/cgrand/enlive) 
- [Clj-tagsoup](https://github.com/r0man/Clj-tagsoup)
- [Clj-htmlparser](https://github.com/glittershark/clj-htmlparser)