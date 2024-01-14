---
title:                "Clojure: Tekstin etsiminen ja korvaaminen"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi sitä haluaisi etsiä ja korvata tekstiä? Etsiminen ja korvaaminen on hyödyllistä, kun haluat nopeasti muuttaa suurta määrää tekstiä koodissa tai tekstitiedostoissa. Se voi myös auttaa virheiden korjaamisessa tai kaavion standardoinnissa.

## Miten tehdä

Käytä Clojuren `clojure.string/replace` -funktiota etsiäksesi ja korvataksesi tekstiä. Esimerkiksi:

```Clojure
(clojure.string/replace "Tervetuloa maailmaan" #"Tervetuloa" "Hei")
```

Tämä koodi korvaa "Tervetuloa" tekstin "Hei", jolloin lopputuloksena on "Hei maailmaan".

## Syvempi sukellus

`clojure.string/replace` hyödyntää [regular expression](https://fi.wikipedia.org/wiki/S%C4%85ne_kirjoitus), joka antaa sinulle enemmän mahdollisuuksia muokata tekstiä. Voit esimerkiksi käyttää wilcard -merkkejä tai ryhmittelyjä etsiessäsi ja korvatessaessasi tekstiä. Tässä on muutamia esimerkkejä:

- `"Hei maailma" #"(.*)" "$1 everyone"` muuttaa "Hei maailma" tekstiksi "Hei kaikille"
- `"Tervetuloa * planeetta" #"(.*)* planeetta" "Hei $1"` muuttaa "Tervetuloa Maapalloon" tekstiksi "Hei Maapallo"
- `"123-456-7890" #"(\d{3})-(\d{3})-(\d{4})" "($1) $2-$3"` muuttaa puhelinnumeron muotoon "(123) 456-7890"

## Katso myös

- [Clojure Docs](https://clojure.org/api/cheatsheet)
- [Regular Expressions in Clojure](https://clojuredocs.org/clojure.core/re-find)
- [Tehokas tekstien muokkaus Clojuressa](https://medium.com/@tonyshaw/tackle-text-manipulation-in-clojure-effectively-9ed811a9b911)