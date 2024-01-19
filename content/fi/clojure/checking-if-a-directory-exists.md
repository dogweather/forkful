---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Clojure: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tyypillisesti, tarkistamme, onko hakemisto olemassa, estääksemme virheitä, joita saattaa ilmetä, jos yritämme käyttää olematonta hakemistoa. Se auttaa meitä varmistamaan, että ohjelma toimii odotetulla tavalla ja ylläpitämään ohjelman luotettavuutta.

## Kuinka:

Klojurella hakemiston olemassaolotarkistus on suoraviivaista. Tässä on esimerkkikoodi.

```Clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [dir]
  (.exists (io/file dir)))
```

Ja sen käyttö:

```Clojure
(directory-exists? "/path/to/directory") 
;=> Tämä palauttaa true, jos hakemisto on olemassa, muuten false
```

## Syvempi Sukellus:

Historiallisesti, ohjelmoijien on täytynyt tarkistaa manuaalisesti tiedostojärjestelmän eheyttä. Clojure, ja sen taustalla toimiva Java-platformi, tarjoavat kuitenkin nämä toiminnallisuudet valmiina.

Clojuren `java.io`-kirjasto mahdollistaa järjestelmätason operaatioiden suorittamisen, kuten hakemiston olemassaolon tarkistuksen, korkean tason funktioilla, mikä tekee toiminnasta järjestelmäriippumatonta.

Kuten aina, on olemassa useita muita tapoja toteuttaa sama toiminto. Eräs Clojuren arkkitehtuurin kauneimmista puolista on, että se antaa ohjelmoijien valita parhaiten sopivan tien. Mikäli haluat tarkistaa tiedoston (eikä hakemiston) olemassaolon, voit käyttää esimerkiksi Clojure `java.nio.file`-kirjaston `Files/exists` -metodia.

## Katso Myös:

Lisätietoja voit saada seuraavista viitteistä:

1. [Clojure's 'java.io' library documentation](https://clojuredocs.org/clojure.java.io)
2. [Java 7 java.nio.file documentation](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html)
3. [StackOverflow: How to check if a file exists? (in Clojure)](https://stackoverflow.com/questions/2028158/how-to-check-if-a-file-exists-in-clojure)
4. [Blog: Clojure file and directory operations](https://kimh.github.io/clojure-by-example/posts-output/2014-11-22-file-and-directory-operations.html)