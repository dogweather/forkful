---
title:                "Tekstitiedoston lukeminen"
html_title:           "Clojure: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi lukea tekstitiedostoa? Tekstitiedostot ovat perinteisiä ja yksinkertaisia tapoja tallentaa ja jakaa tietoa, joten voi olla hyödyllistä tietää, miten niitä voi lukea ja käyttää Clojuren avulla.

## Kuinka

Tekstitiedostojen lukeminen Clojuren avulla on helppoa. Voit käyttää `slurp`-funktiota, joka lukee koko tiedoston sisällön ja palauttaa sen merkkijonona. Voit myös käyttää `line-seq`-funktiota, joka lukee tiedoston rivi kerrallaan ja palauttaa jokaisen rivin listana. Esimerkiksi:

```Clojure
(def text (slurp "tiedosto.txt"))
(println text)

(def lines (line-seq (clojure.java.io/reader "tiedosto.txt")))
(doseq [line lines]
  (println line))
```

Tämän esimerkin output voisi olla:

```
Tervetuloa lukemaan tekstiä!
Tässä on ensimmäinen rivi.
Tässä on toinen rivi.
```

## Syväsukellus

Tekstitiedostojen lukeminen Clojuren avulla käyttää Java:n `java.io`-kirjastoa. Voit käyttää myös muita `clojure.java.io`-nimiavaruuden funktioita, kuten `reader`, `input-stream` ja `reader-seq`. Voit myös käyttää `with-open`-lauseketta, joka automaattisesti sulkee tiedoston käytön jälkeen. Esimerkiksi:

```Clojure
(with-open [rdr (clojure.java.io/reader "tiedosto.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))
```

See also (Katso myös)

- [Oficial Clojure -dokumentointi](https://clojure.org/api/cheatsheet)
- [Clojure for the Brave and True](https://www.braveclojure.com/reading-files/)
- [Videotutoriaali: "Reading and Writing Files with Clojure"](https://www.youtube.com/watch?v=4bI3p7CJkL0)