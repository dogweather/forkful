---
title:                "Arbeiten mit XML"
date:                  2024-01-26T04:29:22.442067-07:00
model:                 gpt-4-0125-preview
simple_title:         "Arbeiten mit XML"

category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/working-with-xml.md"
---

{{< edit_this_page >}}

## Was & Warum?
XML ist eine Auszeichnungssprache zum Kodieren von Dokumenten auf eine Weise, die sowohl für Menschen als auch für Maschinen lesbar ist. Sie ist ein Schlüssel in Webdiensten, Konfigurationsdateien und für den Datenaustausch, da sie Daten in einem strukturierten, hierarchischen Format überträgt.

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
