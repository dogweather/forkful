---
date: 2024-01-26 04:29:18.403170-07:00
description: "Hur man g\xF6r: Clojure erbjuder biblioteket `clojure.data.xml` f\xF6\
  r XML-tolkning och generering. F\xF6rst, l\xE5t oss tolka lite XML."
lastmod: '2024-03-13T22:44:37.549465-06:00'
model: gpt-4-0125-preview
summary: "Clojure erbjuder biblioteket `clojure.data.xml` f\xF6r XML-tolkning och\
  \ generering."
title: Att arbeta med XML
weight: 40
---

## Hur man gör:
Clojure erbjuder biblioteket `clojure.data.xml` för XML-tolkning och generering. Först, låt oss tolka lite XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; Tolka XML-sträng
  (println parsed))
```
Utdata:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

För att generera XML från Clojure-strukturer:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
Utdata:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Fördjupning
XML har funnits ett tag, med start i slutet av 90-talet som en förenklad version av SGML, avsedd för webbdata. Användningen exploderade med teknologier som SOAP och XHTML men fick konkurrens av JSON, som föredras för dess lättvikt och enkelhet.

Clojures tillvägagångssätt för XML håller det funktionellt och datacentrerat, vilket är sant för språkets ethos. `clojure.data.xml` är bara ett alternativ; du har `clojure.xml` för grundläggande behov, och för Java-interoperabilitet kan du använda dig av tungviktare som JAXB eller DOM4J.

Tänk på att prestanda- och minnesanvändningen vid hantering av mycket stora XML-dokument kan vara betungande. Strömmande tolkare som StAX kan hjälpa, men då behöver du gå över till Java-land för dem.

## Se även
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API för XML-behandling (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
