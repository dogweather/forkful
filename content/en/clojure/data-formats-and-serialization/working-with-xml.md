---
date: 2024-01-25 03:39:55.199853-07:00
description: 'How to: Clojure offers the `clojure.data.xml` library for XML parsing
  and emitting. First, let''s parse some XML.'
lastmod: '2024-03-13T22:44:59.767074-06:00'
model: gpt-4-1106-preview
summary: Clojure offers the `clojure.data.xml` library for XML parsing and emitting.
title: Working with XML
weight: 40
---

## How to:
Clojure offers the `clojure.data.xml` library for XML parsing and emitting. First, let's parse some XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; Parse XML string
  (println parsed))
```
Output:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

To emit XML from Clojure structures:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
Output:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Deep Dive
XML's been around the block, kicking off in the late '90s as a simplified subset of SGML, intended for web data. It exploded in usage with technologies like SOAP and XHTML but got a bit of competition from JSON, which is preferred for its lightness and simplicity.

Clojure's approach to XML keeps it functional and data-centric, staying true to the language ethos. `clojure.data.xml` is merely one option; you've got `clojure.xml` for basic needs, and for Java interop, you can swing with heavy-hitters like JAXB or DOM4J.

Keep in mind, the performance and memory overhead when dealing with very large XML documents can be hefty. Streaming parsers like StAX can help, but you'll need to drop into Java-land for them.

## See Also
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
