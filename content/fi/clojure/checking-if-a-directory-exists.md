---
title:                "Tarkistetaan onko hakemistoa olemassa"
html_title:           "Clojure: Tarkistetaan onko hakemistoa olemassa"
simple_title:         "Tarkistetaan onko hakemistoa olemassa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Tarkistaakseni, onko hakemisto olemassa, käytän Clojuren (nykyinen versio) `clojure.java.io/file?` -funktiota. Tämä tarkistaa, onko annetulla polulla oleva tiedosto hakemisto.

Ohjelmoijat tarkistavat hakemiston olemassaolon usein, jotta he voivat varmistaa, että he ovat oikeassa paikassa ja että tarvittavat tiedostot ovat saatavilla. Tämä auttaa välttämään virheitä ja mahdollistaa sujuvan suorituksen.

## Kuinka tehdä?

Voit tarkistaa hakemiston olemassaolon käyttämällä `clojure.java.io/file?` -funktiota. Tässä on esimerkki:

```Clojure
(ns example.core (:require [clojure.java.io :as io]))

(def polku "C:/kansio")

(if (io/file? polku) (println "Hakemisto olemassa.") (println "Hakemistoa ei löydy."))
```

Esimerkin output riippuu siitä, onko annetulla polulla hakemisto vai ei:

```
Hakemisto olemassa.
```

tai

```
Hakemistoa ei löydy.
```

## Syväsukellus

Tarkistus, onko hakemisto olemassa, on yleinen tehtävä ohjelmoinnissa. Ennen `clojure.java.io/file?` -funktion saatavuutta Clojuren käyttäjien piti käyttää Java-luokkaa `java.io.File` tämän toiminnon suorittamiseen.

On myös muita tapoja tarkistaa hakemiston olemassaolo, kuten käyttämällä tiedostojärjestelmän komentoja kuten `ls` tai `dir` ja tarkistamalla, onko hakemisto lista tiedostoista.

`clojure.java.io/file?` -funktio käyttää Clojuren `java.io.File` luokkaa tarkistamalla, onko tiedosto tai hakemisto olemassa ja palauttaa vastaavan totuusarvon `true` tai `false`.

## Katso myös

- [Clojuren virallinen dokumentaatio `clojure.java.io/file?` -funktiosta](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file_qmark)
- [Java-luokka `java.io.File`](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)