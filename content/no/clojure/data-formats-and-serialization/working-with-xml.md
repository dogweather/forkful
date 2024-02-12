---
title:                "Å jobbe med XML"
date:                  2024-01-26T04:29:08.275957-07:00
model:                 gpt-4-0125-preview
simple_title:         "Å jobbe med XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/working-with-xml.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
XML er et merkespråk for koding av dokumenter på en måte som er lesbar både for mennesker og maskiner. Det er nøkkel i webtjenester, konfigurasjonsfiler og datautveksling fordi det bærer data i et strukturert, hierarkisk format.

## Hvordan:
Clojure tilbyr `clojure.data.xml`-biblioteket for XML-parsing og -generering. Først, la oss parse litt XML:

```clojure
(require '[clojure.data.xml :as xml])

(let [innhold "<root><foo>bar</foo><foo>baz</foo></root>"
      parset (xml/parse-str innhold)] ; Parse XML-streng
  (println parset))
```
Output:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

For å generere XML fra Clojure-strukturer:

```clojure
(def min-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str min-xml))
```
Output:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Dypdykk
XML har vært rundt en stund, startet på slutten av 90-tallet som en forenklet delmengde av SGML, ment for webdata. Det eksploderte i bruk med teknologier som SOAP og XHTML, men fikk litt konkurranse fra JSON, som foretrekkes for sin letthet og enkelhet.

Clojures tilnærming til XML holder det funksjonelt og datasentrert, tro mot språkets etos. `clojure.data.xml` er bare ett alternativ; du har også `clojure.xml` for grunnleggende behov, og for Java-interoperabilitet, kan du svinge med tunge slagere som JAXB eller DOM4J.

Husk at ytelses- og minnebelastningen ved å håndtere svært store XML-dokumenter kan være betydelig. Streaming parseren som StAX kan hjelpe, men du må gå over til Java-land for dem.

## Se Også
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API for XML-behandling (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)
