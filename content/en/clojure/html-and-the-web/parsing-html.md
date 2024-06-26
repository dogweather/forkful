---
date: 2024-02-03 19:02:49.120272-07:00
description: "How to: Clojure does not have built-in HTML parsing capabilities, but\
  \ you can leverage Java libraries or Clojure wrappers such as `enlive` or `hickory`.\u2026"
lastmod: '2024-03-13T22:44:59.743143-06:00'
model: gpt-4-0125-preview
summary: Clojure does not have built-in HTML parsing capabilities, but you can leverage
  Java libraries or Clojure wrappers such as `enlive` or `hickory`.
title: Parsing HTML
weight: 43
---

## How to:
Clojure does not have built-in HTML parsing capabilities, but you can leverage Java libraries or Clojure wrappers such as `enlive` or `hickory`. Here's how to use both:

### Using Enlive:
Enlive is a popular choice for HTML parsing and web scraping. First, include it in your project dependencies:

```clojure
[net.cgrand/enlive "1.1.6"]
```

Then, you can parse and navigate HTML like so:

```clojure
(require '[net.cgrand.enlive-html :as html])

(let [doc (html/html-resource (java.net.URL. "http://example.com"))]
  (html/select doc [:div.some-class]))
```

This snippet fetches an HTML page and selects all `<div>` elements with the class `some-class`.

Output might look like:

```clojure
({:tag :div, :attrs {:class "some-class"}, :content ["Here's some content."]})
```

### Using Hickory:
Hickory provides a way to parse HTML into a format that is easier to work with in Clojure. Add Hickory to your project dependencies:

```clojure
[hickory "0.7.1"]
```

Here's a simple example:

```clojure
(require '[hickory.core :as hickory]
         '[hickory.select :as select])

;; Parse the HTML into Hickory format
(let [doc (hickory/parse "<html><body><div id='main'>Hello, world!</div></body></html>")]
  ;; Select the div with id 'main'
  (select/select (select/id "main") doc))
```

This code parses a simple HTML string and uses a CSS selector to find a `div` with the ID `main`.

Sample output:

```clojure
[{:type :element, :tag :div, :attrs {:id "main"}, :content ["Hello, world!"]}]
```

Both `enlive` and `hickory` offer robust solutions for HTML parsing in Clojure, with `enlive` focusing more on templating and `hickory` emphasizing data transformation.
