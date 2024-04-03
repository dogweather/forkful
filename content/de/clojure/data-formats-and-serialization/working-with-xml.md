---
date: 2024-01-26 04:29:22.442067-07:00
description: "XML ist eine Auszeichnungssprache zum Kodieren von Dokumenten auf eine\
  \ Weise, die sowohl f\xFCr Menschen als auch f\xFCr Maschinen lesbar ist. Sie ist\
  \ ein\u2026"
lastmod: '2024-03-13T22:44:53.444581-06:00'
model: gpt-4-0125-preview
summary: "XML ist eine Auszeichnungssprache zum Kodieren von Dokumenten auf eine Weise,\
  \ die sowohl f\xFCr Menschen als auch f\xFCr Maschinen lesbar ist."
title: Arbeiten mit XML
weight: 40
---

## Wie man:
Clojure bietet die Bibliothek `clojure.data.xml` für das Parsen und Erzeugen von XML. Zuerst parsen wir etwas XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; XML-String parsen
  (println parsed))
```
Ausgabe:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Um XML aus Clojure-Strukturen zu erzeugen:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
Ausgabe:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Tiefergehend
XML ist ein alter Hase, der Ende der 90er Jahre als vereinfachter Subset von SGML für Webdaten eingeführt wurde. Seine Nutzung explodierte mit Technologien wie SOAP und XHTML, bekam aber Konkurrenz von JSON, das wegen seiner Leichtigkeit und Einfachheit bevorzugt wird.

Clojures Ansatz zu XML hält es funktional und datenzentriert, getreu dem Ethos der Sprache. `clojure.data.xml` ist nur eine Option; es gibt `clojure.xml` für grundlegende Bedürfnisse und für Java-Interop kann man auf Schwergewichte wie JAXB oder DOM4J zurückgreifen.

Denken Sie daran, dass die Leistungs- und Speicherbelastung beim Umgang mit sehr großen XML-Dokumenten beachtlich sein kann. Streaming-Parser wie StAX können helfen, aber dafür müssen Sie in Java-Land wechseln.

## Siehe auch
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API für XML-Verarbeitung (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
