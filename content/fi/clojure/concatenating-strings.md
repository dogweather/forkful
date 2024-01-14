---
title:                "Clojure: Merkkijonojen yhdistäminen"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit yhdistää merkkijonoja ohjelmoinnissa? Yksinkertaisesti sanottuna, yhdistämällä merkkijonoja voit luoda uusia merkkijonoja, jotka sisältävät haluamasi tiedon.

## Kuinka

Yhdistämisen avulla voit helposti yhdistää kaksi erillistä merkkijonoa yhdeksi. Voit tehdä tämän käyttämällä funktiota nimeltä `str`, joka ottaa vastaan kaksi parametria ja palauttaa uuden merkkijonon.

```Clojure
(str "Tämä on ensimmäinen merkkijono" " ja tämä on toinen.")
```
Tuloksena saamme seuraavan merkkijonon:

```Clojure
"Tämä on ensimmäinen merkkijono ja tämä on toinen."
```

## Syvempi sukellus

On olemassa myös muita tapoja yhdistää merkkijonoja, kuten käyttämällä `str`-funktion sijasta merkkijonojen liittämiseen tarkoitettua operaattoria `++`. Voit myös yhdistää useita merkkijonoja kerrallaan käyttämällä `str`-funktiota yhden parametrin sijasta.

Sinun tulee myös muistaa, että merkkijonojen yhdistäminen voi olla tehokkaampaa kuin merkkijonojen muokkaaminen, jos sinun täytyy tehdä useita muutoksia merkkijonoon.

## Katso myös

- [Clojure.org - Strings](https://clojure.org/reference/strings)
- [Official Clojure Documentation - String Functions](https://clojure.github.io/clojure/clojure.string-api.html)
- [Youtube - Concatenating strings in Clojure](https://www.youtube.com/watch?v=hwZSZY3H1Hc)