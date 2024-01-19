---
title:                "Tilapäisen tiedoston luominen"
html_title:           "Arduino: Tilapäisen tiedoston luominen"
simple_title:         "Tilapäisen tiedoston luominen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tilapäisen tiedoston luominen on ohjelmointiprosessi, jossa luodaan väliaikainen tiedosto datan tallentamiseksi ohjelman ajon aikana. Tätä käytetään suurten datamäärien käsittelyyn, jotka ylittävät suoran muistikäsittelyn.

## Kuinka:

Tässä esimerkissä, kuinka luoda väliaikainen tiedosto Clojurella:

```Clojure
(require '[clojure.java.io :as io])

(defn create-temp-file [prefix suffix]
  (-> (java.io.File/createTempFile prefix suffix)
      (.deleteOnExit)))

(let [temp-file (create-temp-file "tmp" ".txt")]
  (with-open [wtr (io/writer temp-file)]
    (.write wtr "Hello, World!"))
  (slurp temp-file)) 
```      
Tämän suorittaminen tuottaa seuraavan tuloksen:

```Clojure
"Hello, World!"
```

## Syvällisemmin

Historiallisen kontekstin osalta väliaikaisia tiedostoja on käytetty ohjelmoinnissa jo kauan, koska ne mahdollistavat suurten datamäärien käsittelyn ilman, että järjestelmän muisti ylikuormittuu.

Vaihtoehtoisena strategia on luoda väliaikaisia tietokantoja tai käyttää välimuistin käsittelyä. Ne voivat toimia erityisen hyvin tiettyjen sovellusten, kuten suurten verkkopalvelinten, kanssa.

Mitä tulee toteutuksen yksityiskohtiin, nämä väliaikaiset tiedostot tallentuvat järjestelmän määrittämään väliaikaiseen hakemistoon. `(.deleteOnExit)` -metodia käytetään, jotta tiedosto poistetaan automaattisesti, kun virtuaalikone lopettaa.

## Katso myös

[GNU Core Utilities: Temporary Files](https://www.gnu.org/software/coreutils/manual/html_node/Temporary-Files.html#Temporary-Files)

[Java Platform SE: Class File](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)

[Clojure Docs: clojure.java.io/writer](https://clojuredocs.org/clojure.java.io/writer)