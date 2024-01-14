---
title:    "Clojure: Merkkijonon pituuden löytäminen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluat selvittää merkkijonon pituuden? Hyvä kysymys! Merkkijonon pituuden selvittäminen on usein tärkeä osa ohjelmointia, sillä se auttaa meitä käsittämään ja manipuloimaan tekstejä.

## Miten

```Clojure
(println "Syötä haluamasi merkkijono:")
(def merkkijono (read-line))
(println "Merkkijonon pituus on" (count merkkijono) "merkkiä.")
```

Esimerkkisyöte: Hei maailma!

Tuloste: Merkkijonon pituus on 12 merkkiä.

## Syvempää tietoa

Merkkijonon pituus voidaan selvittää Clojuressa käyttämällä `count`-funktiota, joka palauttaa merkkijonon merkkien määrän. On kuitenkin tärkeää muistaa, että `count` laskee myös välilyönnit ja muut erikoismerkit osaksi merkkien lukumäärää. Lisäksi Clojure tarjoaa muitakin tapoja käsitellä ja manipuloida merkkijonoja, kuten `subs`-funktio, joka palauttaa osan merkkijonosta, ja `str`-funktio, joka yhdistää useita arvoja yhdeksi merkkijonoksi.

## Katso myös

- [Clojure Docs: Count](https://clojuredocs.org/clojure.core/count)
- [Clojure Docs: Subs](https://clojuredocs.org/clojure.core/subs)
- [Clojure Docs: Str](https://clojuredocs.org/clojure.core/str)