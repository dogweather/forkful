---
title:                "Clojure: Tilapäistiedoston luominen"
simple_title:         "Tilapäistiedoston luominen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Miksi luoda tilapäistiedosto

Monissa ohjelmointiprojekteissa on tarvetta luoda tilapäisiä tiedostoja, esimerkiksi väliaikaisesti tallentamaan tietoa tai suoritettavien toimintojen välivarastointiin. Tässä blogikirjoituksessa tarkastelemme, miten luoda tilapäistiedostoja Clojure-kielellä.

## Miten luoda tilapäistiedosto

Tilapäistiedostoja voi luoda käyttämällä Clojuren standardikirjaston `java.io.File` -kirjastoa. Tässä esimerkissä luomme tilapäistiedoston nimeltä "temp.txt" ja kirjoitamme siihen "Hello world!" -tekstin.

```Clojure
(import [java.io File])

(def f (File/createTempFile "temp" ".txt"))
(.write f "Hello world!")
```

Tämän jälkeen voimme lukea tiedoston sisällön ja tulostaa sen konsoliin.

```Clojure
(def contents (slurp (.getAbsolutePath f)))
(println contents)
```

Tämän koodin tulosteena pitäisi olla "Hello world!".

## Syvällisempi sukellus

Tähän asti olemme käyttäneet `java.io.File` -kirjastoa luomaan tilapäistiedostoja. Tämä kirjasto tarjoaa kuitenkin vain rajallisen määrän toimintoja tilapäistiedostojen käsittelyyn. Jos haluamme enemmän joustavuutta, voimme käyttää esimerkiksi `clojure.java.io` -kirjastoa.

```Clojure
(import [clojure.java.io FileFilter])

(defn my-file-filter [^File file]
  (.getName file)    ; Palauttaa tiedoston nimen
)

(def f (with-temporary-file "test.txt" #(println %)))    ; Luo temp-tiedoston ja suorittaa annetun funktion

(def temp-files (temp-files         ; Palauttaa listan sijainneista, joissa tilapäistiedostot on tallennettu tiedostojärjestelmään
                  (re-find #"test.*" (into-array File (temp-files)))))
```

`clojure.java.io` -kirjasto tarjoaa monipuolisemmat toiminnot tilapäistiedostojen luomiseen ja käsittelyyn, joten suosittelemme tutustumaan siihen lisää.

## Katso myös

- [Clojuren virallinen dokumentaatio](https://clojure.github.io/java.io-api/clojure.java.io.html#var-with-open)
- [Java:n virallinen dokumentaatio](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)