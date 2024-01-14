---
title:                "Clojure: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi kirjoittaa tekstifilen Clojurella? Textfiles ovat kätevä tapa tallentaa ja jakaa tietoa, kuten tekstejä, listaamista ja dataa. Ne tarjoavat myös mahdollisuuden monipuolisiin manipulointeihin ja muokkauksiin.

## Kuinka tehdä

```Clojure
(def file (slurp "tekstifile.txt")) ; lataa tekstitiedoston "tekstifile.txt"

(println file) ; tulostaa tekstin tekstifile.txt
```

## Syvällisempi sukellus

Textfilen luominen Clojurella alkaa yksinkertaisella "slurp" -funktiolla, joka lataa tiedoston sisällön muuttujaan. Tämän jälkeen voit käyttää muuttujaa tulostamaan, muokkaamaan tai tallentamaan tietoa.

Voit myös käyttää "spit" -funktiota luodaksesi uuden tekstitiedoston ja tallentaa siihen haluamasi sisällön.

```Clojure
(spit "uusi_teksti.txt" "Tämä on uusi tekstifile luotu Clojurella")
```

## Katso myös

- [Clojure - dokumentaatio](https://clojure.org/)
- [Tekstifilejen luominen Clojurella](https://github.com/clojure-cookbook/clojure-cookbook/blob/master/02_language/7_reading-and-writing.texts/7_03_writing_md.md)