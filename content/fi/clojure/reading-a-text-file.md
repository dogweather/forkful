---
title:                "Clojure: Tiedostotekstin lukeminen"
simple_title:         "Tiedostotekstin lukeminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmointitehtävissä kohtaat tarpeen lukea tekstiä sisältävä tiedosto ohjelman käsittelyä varten. Tämä blogikirjoitus antaa sinulle tarvittavat tiedot käsitellä tekstiä Clojure-ohjelmointikielellä.

## Kuinka

Käytä Clojuren standardikirjastosta löytyvää `clojure.java.io`-kirjastoa tekstitiedoston lukemiseen. Käytä `slurp`-funktiota ja anna polku tekstiä sisältävään tiedostoon parametrina.

```Clojure
(require '[clojure.java.io :as io])

(slurp "tiedostonimi.txt")
```

Tämä palauttaa tekstiä sisältävän tiedoston sisällön merkkijonona.

```
Hello World!
Tämä on esimerkki tekstiä sisältävästä tiedostosta.
```

## Syventävä sukellus

Voit myös käyttää `with-open`-lauseketta yhdessä `reader`-funktion kanssa lukeaksesi tiedoston rivi kerrallaan.

```Clojure
(with-open [reader (io/reader "tiedostonimi.txt")]
  (doseq [line (line-seq reader)]
    (println line)))
```

Tämä tulostaisi tekstin tiedoston sisällön rivit erikseen.

```
Hello World!
Tämä on esimerkki tekstiä sisältävästä tiedostosta.
```

## Katso myös

- [clojure.java.io - ClojureDocs](https://clojuredocs.org/clojure.java.io)
- [Text Files in Clojure - Clojure for the Brave and True](https://www.braveclojure.com/working-with-text-files/)