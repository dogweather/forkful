---
title:                "Werken met XML"
date:                  2024-01-28T22:11:10.439480-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/clojure/working-with-xml.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
XML is een opmaaktaal voor het coderen van documenten op een wijze die zowel door mensen als machines gelezen kan worden. Het is cruciaal in webservices, configuratiebestanden en gegevensuitwisseling omdat het gegevens draagt in een gestructureerd, hiërarchisch formaat.

## Hoe:
Clojure biedt de `clojure.data.xml` bibliotheek voor XML-parsing en -generatie. Laten we eerst wat XML parsen:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; Parseer XML string
  (println parsed))
```
Output:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

Om XML te genereren vanuit Clojure-structuren:

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

## Diepgaand
XML bestaat al een tijdje, begonnen in de late jaren '90 als een vereenvoudigde subset van SGML, bedoeld voor webgegevens. Het explodeerde in gebruik met technologieën zoals SOAP en XHTML maar kreeg concurrentie van JSON, dat de voorkeur heeft vanwege de lichtheid en eenvoud.

Clojure's benadering van XML houdt het functioneel en data-gericht, trouw blijvend aan de ethos van de taal. `clojure.data.xml` is slechts één optie; je hebt `clojure.xml` voor basisbehoeften, en voor Java-interoperabiliteit, kun je gaan met zwaargewichten zoals JAXB of DOM4J.

Houd er rekening mee dat de prestaties en geheugenoverhead bij het omgaan met zeer grote XML-documenten zwaar kunnen zijn. Streaming parsers zoals StAX kunnen helpen, maar daarvoor moet je wel naar Java-land.

## Zie Ook
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
