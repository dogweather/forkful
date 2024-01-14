---
title:                "Clojure: Alirivien erottaminen"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringin paloittelu (engl. 'extracting substrings') on tärkeä taito Clojure-ohjelmoinnissa, sillä se mahdollistaa tehokkaan ja tarkan datan käsittelyn. Oli sitten kyseessä teksti, numero tai lista, substringien avulla voit helposti valita halutut osat datasta ja käsitellä niitä haluamallasi tavalla.

## Miten

Substringin paloittelun käyttöönotto onnistuu kätevästi Clojuren sisäänrakennetun `subs`-funktion avulla. Tämän funktion avulla voit luoda uuden merkkijonon, joka koostuu valituista osista alkuperäisestä merkkijonosta.

```Clojure
(def teksti "Tervetuloa Clojure-maailmaan!")

(subs teksti 4 14) ; tulostaa "tuloa Cloj"
(subs teksti 17) ; tulostaa "a-maailmaan!"
```
`subs` ottaa vastaan kolme argumenttia: merkkijonon, aloitusindeksin ja lopetusindeksin. Jos lopetusindeksiä ei anneta, `subs` käyttää merkkijonon loppua.

`subs`-funktion lisäksi voit myös käyttää `slice`-funktiota, joka toimii samalla tavalla kuin `subs`, mutta käyttää indeksien sijaan aloitus- ja lopetusarvoja.

```Clojure
(def lista [1 2 3 4 5])

(slice lista 1 3) ; tulostaa [2 3]
(slice lista 3) ; tulostaa [4 5]
```

## Syvereihin

Syvennytään hieman tarkemmin substringien paloitteluun Clojuren sisällä. Aiemmin mainitut `subs` ja `slice` ovat hyödyllisiä yksinkertaisiin substringitoimintoihin, mutta jos tarvitset monimutkaisempia toimintoja, voi olla hyvä tutustua `re-seq`-funktioon. Tämän funktion avulla voit etsiä merkkijonosta kaikki halutut alimerkkijonot käyttämällä säännöllisiä lausekkeita.

```Clojure
(def teksti2 "Tänään on lauantai")

(re-seq #"^[^aeiou]+" teksti2) ; tulostaa ("T" "n")
```

Tässä esimerkissä säännöllinen lauseke etsii merkkijonosta kaikki kirjaimet, jotka eivät ole vokaaleja, ja palauttaa nämä alimerkkijonot listana.

## Katso myös

- [Clojure dokumentaatio](https://clojure.org/reference/strings)
- [säännölliset lausekkeet Clojurella](https://clojure.org/guides/regular_expressions)