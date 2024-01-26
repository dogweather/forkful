---
title:                "XML:n käsittely"
date:                  2024-01-26T04:29:00.132074-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML:n käsittely"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-xml.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
XML on merkintäkieli dokumenttien koodaamiseen tavalla, joka on sekä ihmis- että koneellisesti luettavissa. Se on avainasemassa verkkopalveluissa, konfiguraatiotiedostoissa ja datan vaihdossa, koska se kuljettaa tietoa rakenteellisessa, hierarkisessa muodossa.

## Kuinka:
Clojure tarjoaa `clojure.data.xml` kirjaston XML:n jäsentämiseen ja tuottamiseen. Aloitetaan XML:n jäsentämisestä:

```clojure
(require '[clojure.data.xml :as xml])

(let [content "<root><foo>bar</foo><foo>baz</foo></root>"
      parsed (xml/parse-str content)] ; Jäsennä XML-merkkijono
  (println parsed))
```
Tuloste:
```
Element{:tag :root, :attrs {}, :content (Element{:tag :foo, :attrs {}, :content ("bar")} Element{:tag :foo, :attrs {}, :content ("baz")})}
```

XML:n tuottamiseksi Clojure-rakenteista:

```clojure
(def my-xml (xml/element :root {}
                          (xml/element :foo {} "bar")
                          (xml/element :foo {} "baz")))

(println (xml/emit-str my-xml))
```
Tuloste:
```
<root><foo>bar</foo><foo>baz</foo></root>
```

## Syväsukellus
XML on ollut olemassa jo kauan, alkaen 90-luvun lopulla yksinkertaistettuna SGML:n alajoukkona web-datan käyttöön. Sen käyttö räjähti teknologioiden kuten SOAP:n ja XHTML:n myötä, mutta sai kilpailua JSON:sta, jota suositaan sen keveyden ja yksinkertaisuuden vuoksi.

Clojuren lähestymistapa XML:ään pitää sen funktionaalisena ja datakeskeisenä, pysyen uskollisena kielen eetokselle. `clojure.data.xml` on vain yksi vaihtoehto; perustarpeisiin sopii `clojure.xml`, ja Java-yhteensopivuuteen voit turvautua raskassarjalaisiin kuten JAXB tai DOM4J.

Pidä mielessä, että erittäin suurten XML-dokumenttien käsittelyn suorituskyky- ja muistivaatimukset voivat olla raskaat. Streaming-jäsentäjät kuten StAX voivat auttaa, mutta niiden käyttöön joudut siirtymään Java-maailmaan.

## Katso Myös
- [clojure.data.xml GitHub](https://github.com/clojure/data.xml)
- [Java API for XML Processing (JAXP)](https://docs.oracle.com/javase/tutorial/jaxp/index.html)
- [StAX](https://docs.oracle.com/javase/tutorial/jaxp/stax/index.html)