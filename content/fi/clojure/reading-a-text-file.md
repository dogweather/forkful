---
title:                "Tiedoston lukeminen"
html_title:           "Clojure: Tiedoston lukeminen"
simple_title:         "Tiedoston lukeminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lukeminen teksti-tiedostosta tarkoittaa tiedon lukemista tiedostosta, joka sisältää tekstiä. Ohjelmoijat tekevät tätä saadakseen pääsyn tiedostossa olevaan tietoon ja käyttävät sitä sitten lukuisiin tarkoituksiin.

## Kuinka:
Esimerkki koodia teksti-tiedoston lukemisesta ja tulostamisesta käyttäen Clojurea:
```Clojure
(with-open [reader (clojure.java.io/reader "tiedostonimi.txt")]
  (doseq [line (line-seq reader)]
    (println line)))
```
Esimerkki tulostuksesta, kun tiedostossa on seuraava teksti:
```
Tervetuloa Clojure-maailmaan!
Tämä on teksti-tiedosto.
```
(Output)
```
Tervetuloa Clojure-maailmaan!
Tämä on teksti-tiedosto.
```

## Syvemmälle:
Teksti-tiedostojen lukemisen taustalla on tietokoneiden kyky käsitellä ja tallentaa teksti-pohjaista tietoa. Clojuren lisäksi myös muut ohjelmointikielet, kuten Java, tarjoavat mahdollisuuden lukea teksti-tiedostoja. Tämä voi olla hyödyllistä esimerkiksi tiedon tallentamiseen ja käsittelyyn ohjelmien sisällä. Tiedostojen lukemisen lisäksi niitä voidaan myös muokata ja tallentaa takaisin alkuperäiseen tiedostoon.

## Katso myös:
Lisätietoa lukemisen teksti-tiedostoista ja niiden sisältämän tiedon käsittelemisestä löytyy seuraavista lähteistä:
- [Clojure - lukeminen tiedostosta](https://clojure.org/reference/java_interop#_reading_from_a_file)
- [Java - luokka InputStreamReader](https://docs.oracle.com/javase/8/docs/api/java/io/InputStreamReader.html)
- [Muut ohjelmointikielet - teksti-tiedostojen lukeminen](https://en.wikibooks.org/wiki/Programming:Fundamental_Programming_Concepts/Files)