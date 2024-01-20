---
title:                "Parsing html"
date:                  2024-01-20T15:30:55.511340-07:00
html_title:           "Bash recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML is the act of turning a string of HTML into a data structure your program can understand and manipulate. Programmers do it to interact with, extract, and modify content from the web.

## How to:

To parse HTML in Clojure, we use the `clj-tagsoup` library, a wrapper for the Tagsoup Java library which is handy for parsing real-world HTML.

First, add the clj-tagsoup dependency to your project:

```clojure
[clj-tagsoup "0.3.3"] ; Check for the latest version
```

Now, let's parse some HTML:

```clojure
(require '[clj-tagsoup.core :as tagsoup])

; Parse HTML and get a vector of maps representing the parsed elements
(def parsed-html (tagsoup/parse-string "<html><body><p>Hello, World!</p></body></html>"))

; Access elements
(println (first parsed-html))
```

Sample output:

```clojure
{:tag :html, :attrs {}, :content [...]}
```

To extract specific elements, like paragraphs:

```clojure
(defn extract-paragraphs [html]
  (let [parsed (tagsoup/parse-string html)]
    (filter #(= :p (:tag %)) parsed)))

; Usage
(extract-paragraphs "<p>First</p><p>Second</p>")
```

## Deep Dive

Parsing HTML in Clojure, as with other languages, typically involves navigating a tree-like structure. Back in the day, this could get messy. Libraries like Tagsoup made life easier by handling quirky real-world HTML.

Clojure’s functional nature lets us manipulate HTML data smoothly. Libraries like `clj-tagsoup` leverage Java's battle-tested tools while adding Clojure's elegance.

Alternative libraries include `Enlive` and `Hickory`. Enlive specializes in both parsing and templating, allowing more complex operations. Hickory translates HTML to Clojure data structures for those who prefer a pure Clojure solution.

The implementation focuses on ease and a declarative style. Under the hood, `clj-tagsoup` uses locators and navigators to traverse HTML, providing a higher abstraction over direct DOM manipulation.

## See Also

- clj-tagsoup on GitHub: https://github.com/nathell/clj-tagsoup
- Tagsoup, the underlying Java library: https://github.com/McCLIM/cl-tagsoup
- Enlive, another Clojure HTML parsing library: https://github.com/cgrand/enlive
- Hickory, a Clojure project for parsing HTML: https://github.com/davidsantiago/hickory