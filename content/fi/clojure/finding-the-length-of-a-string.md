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

Mitä ja miksi?

Pituuden löytäminen merkkijonosta on yksinkertainen mutta tärkeä tehtävä Clojure-ohjelmoinnissa. Ohjelmoijat käyttävät tätä toimintoa usein vertaillessaan merkkijonoja tai laskiessaan niiden arvoja.

Kuinka teet sen:

```Clojure
(length "Hello World")
```

Tulos: 11

Voit myös käyttää `count`-funktiota, joka suorittaa saman tehtävän.

```Clojure
(count "Hello World")
```

Tulos: 11

Syötteenä voi olla myös lista, joka sisältää merkkijonoja. Tällöin `count`-funktio laskee kaikkien merkkijonojen yhteispituuden.

```Clojure
(count ["Hello" "World"])
```

Tulos: 10

Poweruserit voivat myös käyttää `map`-funktiota ja muuntaa merkkijonot listoiksi ennen `count`-funktion käyttämistä.

Syväriska:

Pituuden laskemista ei pidä sekoittaa merkkijonon nollapituuden tarkistamiseen. Tämä tapahtuu käyttämällä `empty?`-funktiota.

Alternatiiveja ovat mm. `clojure.string/count` ja `clojure.string/count-char`. Näillä on sama toiminnallisuus kuin `count`-funktiolla, mutta ne ovat osa `clojure.string`-kirjastoa ja saattavat tuoda lisäetuja käytettäväksi.

Voit käyttää myös Java-kirjaston `java.lang.String`-metodia `.length` pituuden laskemiseen.

Katso myös:

https://clojuredocs.org/clojure.core/count
https://clojuredocs.org/clojure.string/count-char