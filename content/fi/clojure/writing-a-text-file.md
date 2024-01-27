---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstitiedoston kirjoittaminen tarkoittaa dataa sisältävän tiedoston luomista tallennusvälineelle. Ohjelmoijat tekevät sitä tiedon säilyttämiseksi, analysointia varten tai kommunikaatioon muiden ohjelmien kanssa.

## How to:
Käytä `spit` funktiota tekstin kirjoittamiseen tiedostoon:

```clojure
(spit "tervehdys.txt" "Moi Clojure-maailma!")
```

Tarkista kirjoitus luomalla ja lukemalla tiedosto:

```clojure
(slurp "tervehdys.txt")
; => "Moi Clojure-maailma!"
```

Kirjoita useita rivejä lisäämällä:

```clojure
(spit "tervehdys.txt" "Toinen rivi." :append true)
```

## Deep Dive
Clojuressa tekstitiedostoihin kirjoittaminen pohjautuu Javan I/O-kirjastoon. Ennen `spit`:iä tiedostoja kirjoitettiin avaten `FileWriter` ja käsikirjoittamalla bufferointi. Vaihtoehtoja `spit`:lle ovat esim. `java.io.PrintWriter`, joka voi formatoida dataa, ja `with-open` rakenne resurssien hallintaan.

## See Also
- ClojureDocs `spit`: https://clojuredocs.org/clojure.core/spit
- Java I/O Tutorial: https://docs.oracle.com/javase/tutorial/essential/io/
- Clojure `with-open`: https://clojuredocs.org/clojure.core/with-open
