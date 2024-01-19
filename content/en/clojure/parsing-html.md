---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML simply means interpreting HTML documents into a structure a computer can understand. Programmers do it to manipulate, extract or change webpage contents automatically.

## How to:

In Clojure, libraries like Enlive and Hickory make HTML parsing easier. Here's a basic example using Enlive:

```Clojure
(require '[net.cgrand.enlive-html :as html])

(defn parse-html [html-content]
  (-> html-content
      java.io.StringReader.
      html/html-resource
      html/select [[:p]]))

(println (parse-html "<p>Hello, Clojure!</p>"))
```

Running this code will print the text inside the paragraph tag:

`[Hello, Clojure!]`

## Deep Dive

Parsing HTML in Clojure is not traditionally done by built-in functions but with third-party libraries. Enlive, used above, offers parser and selector capabilities and has been around since Clojure 1.2. Alternatively, Hickory offers Clojure idioms for HTML as data and is praised for its simplicity.

Internally, these libraries implement HTML parsing via Java libraries (like Jsoup for Enlive and TagSoup for Hickory). An HTML document is converted to a parse tree, which Clojure can manipulate as a simple data structure.

## See Also

For further reading, here are some great resources:

1. [Enlive GitHub](https://github.com/cgrand/enlive) - For advanced usage and detailed documentation.

2. [Hickory GitHub](https://github.com/davidsantiago/hickory) - For further understanding on HTML parsing in the Hickory style.

3. [Clojure for the Brave and True](https://www.braveclojure.com/) - Provides a good starting point for those new to Clojure.

4. [StackOverflow: Parsing HTML](https://stackoverflow.com/questions/5374495/parsing-html-in-clojure) - A discussion on various ways to parse HTML in Clojure.