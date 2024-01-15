---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Clojure: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi haluat löytää merkkijonon pituuden, kuten tarkistaa syötteiden validiuden, manipuloida tekstejä tai vain yksinkertaisesti saada tietoa syötteestäsi.

## Miten

Onneksi Clojurella on sisäänrakennettu funktio nimeltään "count" joka laskee merkkijonon pituuden. Esimerkiksi:

```Clojure
(count "Moi maailma!")
```
Tulos olisi 13, koska merkkijonossa on 13 merkkiä. Voit myös käyttää "count" funktiota muissa tietorakenteissa, kuten listoissa ja vektoreissa. Esimerkiksi:

```Clojure
(count [1 2 3 4 5])
```

Tulos olisi 5, koska listassa on viisi alkiota.

## Syvempi sukellus

"count" funktiota voidaan käyttää myös luomaan oman merkkijonon pituusfunktio. Esimerkiksi:

```Clojure
(defn merkkijonon-pituus [merkkijono]
  (count merkkijono))
```
Tämä luo uuden funktion nimeltä "merkkijonon-pituus", joka ottaa yhden argumentin, merkkijonon, ja käyttää "count" funktiota laskemaan sen pituuden. Nyt voimme kutsua tätä funktiota haluamallamme merkkijonolla ja saada saman tuloksen kuin "count" funktion käytössä.

## Katso myös

- [Clojure dokumentaatio](https://clojure.org/guides/getting_started)
- [Clojure Cheat Sheet](https://clojure.org/api/cheatsheet)