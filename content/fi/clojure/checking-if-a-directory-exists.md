---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "C: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja Miksi?
Tarkistamme onko hakemisto olemassa, koska väärässä paikassa suoritettu toiminto voi aiheuttaa virheitä. Haluamme varmistaa, että data menee oikeaan paikkaan ja että sovelluksemme käyttäytyy odotetusti.

## How to: - Kuinka:
```Clojure
(ns example.dir-check
  (:require [clojure.java.io :as io]))

(defn directory-exists? [dir-path]
  (.exists (io/as-file dir-path)))

;; Esimerkin käyttö:
(println "Does the directory exist? " (directory-exists? "path/to/your/dir"))
```
Jos hakemiston polku on olemassa, tulostus on `true`, muuten `false`.

## Deep Dive - Syväsukellus:
Historiallisesti Clojuren tapa tarkistaa hakemistojen olemassaolo perustuu Javan tiedostojen käsittelyyn. Vaihtoehtoisesti voisi käyttää Javan NIO-pakettia, mutta clojure.java.io tarjoaa yksinkertaisemman ja clojuremaisen tavan tehdä saman.

Clojuren filosofiassa funktiot ovat pääosasassa, ja niinpä 'directory-exists?' kapseloi yleisen tehtävän ytimekkäästi. Javaa käytettäessä vastaavasta toiminnallisuudesta vastaisi 'java.io.File' luokan 'exists()' metodi.

On tärkeää ymmärtää, että hakemiston olemassaolon tarkistus on hetkellinen: hakemistosta voidaan tarkistaa sen olemassaoloa välittömästi ennen toimintoa, mutta tosimaailmassa tilanteet muuttuvat nopeasti. Konkreettinen hakemisto voi kadota tai syntyä uudestaan millä hetkellä tahansa (käyttöjärjestelmän tai toisen sovelluksen toimesta), joten sovelluksen tulisi olla valmis käsittelemään näitä tilanteita dynaamisesti.

Uudemmissa Clojure-versioissa ei ole odotettavissa merkittäviä muutoksia tiedostojen olemassaolon tarkistamismekanismeihin, koska tämä toiminnallisuus on jo vakiintunut ja yksinkertainen.

## See Also - Katso Myös:
- Clojuren virallinen opas tiedostoille: [https://clojure.org/guides/deps_and_cli](https://clojure.org/guides/deps_and_cli)
- Java NIO Files -luokka (vaihtoehtoinen tapa): [https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- ClojureDocs, käytännön esimerkkejä: [https://clojuredocs.org/](https://clojuredocs.org/)
