---
date: 2024-01-20 17:54:05.962758-07:00
description: "How to: \"N\xE4in teet:\" Tiedoston lukeminen kokonaisuudessaan."
lastmod: '2024-04-05T21:53:57.757648-06:00'
model: gpt-4-1106-preview
summary: "\"N\xE4in teet:\" Tiedoston lukeminen kokonaisuudessaan."
title: Tekstitiedoston lukeminen
weight: 22
---

## How to:
"Näin teet:"

Tiedoston lukeminen kokonaisuudessaan:

```Clojure
(slurp "esimerkki.txt")
```

Rivien lukeminen yksi kerrallaan:

```Clojure
(with-open [rdr (clojure.java.io/reader "esimerkki.txt")]
  (doseq [line (line-seq rdr)]
    (println line)))
```

Tulostus voi näyttää tältä:

```
Ensimmäinen rivi tekstiä
Toinen rivi tekstiä
Kolmas rivi tekstiä
```

## Deep Dive
"Sukellus syvemmälle":

Clojuren `slurp` lukaisee koko tiedoston muistiin kerralla. Tämä on nopeaa ja kätevää pienten tiedostojen kanssa. Pitkien tiedostojen kanssa muisti voi kuitenkin loppua, joten silloin `line-seq` ja `reader` ovat parempia vaihtoehtoja, koska ne käsittelevät tiedoston rivi riviltä.

Historiallisesti, lukuoperaatiot juontavat juurensa ohjelmoinnin alkuhämäristä, jolloin tiedon persistenssi levyillä oli ratkaiseva edistysaskel. Clojure puolestaan suosii funktionaalista tapaa käsitellä tiedostoja, mikä näkyy sen keräilyfunktioiden ja laiskan evaluaation hyödyntämisessä.

Vaihtoehtoisesti, voimme käyttää `clojure.java.io` -kirjaston funktioita, jotka tarjoavat Java-interoperabiliteettia – mahdollisuuden käyttää Javan IO-kirjastoa.

## See Also
"Muita lähteitä":

- [Clojure documentation for slurp](https://clojuredocs.org/clojure.core/slurp)
- [Clojure documentation for line-seq](https://clojuredocs.org/clojure.core/line-seq)
- [Clojure for the Brave and True](https://www.braveclojure.com/) - Kirja Clojure-ohjelmoinnista
- [Clojure from the ground up: Welcome](https://aphyr.com/posts/301-clojure-from-the-ground-up-welcome) - Clojure-ohjelmoinnin perusteet
