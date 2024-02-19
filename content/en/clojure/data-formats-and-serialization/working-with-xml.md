---
aliases:
- /en/clojure/working-with-xml/
date: 2024-01-25 03:39:55.199853-07:00
description: "XML is a markup language for encoding documents in a way that is both\
  \ human- and machine-readable. It's key in web services, configuration files, and\
  \ data\u2026"
lastmod: 2024-02-18 23:09:10.747853
model: gpt-4-1106-preview
summary: "XML is a markup language for encoding documents in a way that is both human-\
  \ and machine-readable. It's key in web services, configuration files, and data\u2026"
title: Working with XML
---

{{< edit_this_page >}}

## What & Why?
XML is a markup language for encoding documents in a way that is both human- and machine-readable. It's key in web services, configuration files, and data interchange because it carries data in a structured, hierarchical format.

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
