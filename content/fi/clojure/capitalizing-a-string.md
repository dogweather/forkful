---
title:                "Merkkijonon kirjoittaminen isoin kirjaimin"
html_title:           "Clojure: Merkkijonon kirjoittaminen isoin kirjaimin"
simple_title:         "Merkkijonon kirjoittaminen isoin kirjaimin"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Capitalize on merkkijonon muuntaminen siten, että sen ensimmäinen kirjain muutetaan isoksi. Ohjelmoijat tekevät tämän usein paremman luettavuuden ja yhtenäisyyden vuoksi.

## Miten:
```Clojure
(use 'clojure.string)
(clojure.string/capitalize "hello world")
;; Output: "Hello world"
```

```Clojure
(clojure.string/capitalize "a random sentence")
;; Output: "A random sentence"
```

## Syväsukellus:
- Historiallinen konteksti: ennen kuin Clojure tarjosi tähän funktiota, ohjelmoijat joutuivat luomaan oman funktionsa tai käyttämään muun ohjelmointikielen vastaavaa funktiota.
- Vaihtoehdot: muita tapoja isojen kirjainten asettamiseen ovat `lower-case` ja `upper-case`.
- Toteutuksen yksityiskohdat: Clojuren `capitalize`-funktio käyttää `java.lang.Character`-luokkaa muuttaakseen merkkijonon ensimmäisen kirjaimen isoksi.

## Katso myös:
- [clojure.string | ClojureDocs](https://clojuredocs.org/clojure.string)
- [clojure.string/capitalize | ClojureDocs](https://clojuredocs.org/clojure.string/capitalize)
- [java.lang.Character | Oracle](https://docs.oracle.com/javase/8/docs/api/java/lang/Character.html)