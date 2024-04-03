---
date: 2024-01-26 04:29:27.152667-07:00
description: "Jak to zrobi\u0107: Clojure oferuje bibliotek\u0119 `clojure.data.xml`\
  \ do parsowania i emitowania XML. Najpierw sparsujmy troch\u0119 XML."
lastmod: '2024-03-13T22:44:35.022180-06:00'
model: gpt-4-0125-preview
summary: "Clojure oferuje bibliotek\u0119 `clojure.data.xml` do parsowania i emitowania\
  \ XML."
title: Praca z XML
weight: 40
---

## Jak to zrobić:
Clojure oferuje bibliotekę `clojure.data.xml` do parsowania i emitowania XML. Najpierw sparsujmy trochę XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; Parsowanie ciągu XML
  (println parsed))
```
Wyjście:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Aby emitować XML z struktur Clojure:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
Wyjście:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Wnikliwe spojrzenie
XML krąży po branży od późnych lat '90, zaczynając jako uproszczony podzbiór SGML, przeznaczony dla danych internetowych. Jego użycie wybuchło wraz z technologiami takimi jak SOAP i XHTML, ale otrzymał pewną konkurencję od JSON, który jest preferowany ze względu na lekkość i prostotę.

Podejście Clojure do XML jest funkcjonalne i skupione na danych, pozostając wiernym etosowi języka. `clojure.data.xml` to tylko jedna z opcji; masz też `clojure.xml` dla podstawowych potrzeb, a dla interoperacyjności z Javą możesz sięgnąć po potężne narzędzia takie jak JAXB czy DOM4J.

Należy pamiętać, że wydajność i obciążenie pamięciowe przy bardzo dużych dokumentach XML mogą być znaczne. Parsery strumieniowe, takie jak StAX, mogą pomóc, ale będziesz musiał zapuścić się do świata Javy, aby ich użyć.

## Zobacz także
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API dla przetwarzania XML (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
