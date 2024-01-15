---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "Clojure: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi
Jos olet kiinnostunut ohjelmoinnista, saatat haluta oppia lisää Clojuresta, joka on dynaaminen ohjelmointikieli, joka on suunniteltu toimimaan Java-virtuaalikoneella. Clojurella voit kirjoittaa tekstitiedostoja ja käyttää niitä osana projektejasi.

## Kuinka
Käytä "write-string" -funktiota luodaksesi tekstitiedoston ja "close" -funktiota lopettaaksesi sen käytön. Tässä on esimerkki, joka luo "testi.txt" -tiedoston, kirjoittaa siihen "Tämä on tekstiä!" ja sitten sulkee sen:

```Clojure
(with-open [out-file (clojure.java.io/writer "testi.txt")]
  (.write out-file "Tämä on tekstiä!"))
```

Jos katsot "testi.txt" -tiedostoasi, näet siellä olevan tekstin. Voit myös käyttää "println" -funktiota kirjoittaaksesi rivin käyttäen välilyöntiä tai sanoman lopussa olevaa "newline" -funktiota.

## Syvällinen sukellus
Clojurella on muitakin tapoja luoda ja kirjoittaa tekstitiedostoja, kuten käyttämällä "spit" -funktiota tai "with-open-writer" -makroa. Voit myös antaa lisäargumentteja, kuten tiedoston olemassaolon tarkistamisen, tiedoston oikeuksien asettamisen ja tiedoston sijainnin määrittämisen.

## Katso myös
- Clojure:n viralliset kotisivut: https://clojure.org/
- Clojure-oppikirja: https://clojure-doc.org/
- Clojure:n virallinen dokumentaatio: https://clojuredocs.org/