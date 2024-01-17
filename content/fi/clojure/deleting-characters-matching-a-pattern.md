---
title:                "Mallien määritelmän täsmäävien merkkien poistaminen."
html_title:           "Clojure: Mallien määritelmän täsmäävien merkkien poistaminen."
simple_title:         "Mallien määritelmän täsmäävien merkkien poistaminen."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Poistaminen merkkijonosta voi äkkiseltään kuulostaa hämmentävältä, mutta kyseessä on yksinkertaisesti tietyllä kaavalla mätsäävien merkkien poistaminen merkkijonosta. Tämä on hyödyllistä esimerkiksi siivoamisessa tai tiettyjen merkkien välttämisessä.

## Miten tehdä?
```Clojure
; Poistetaan kaikki numerot merkkijonon alusta
(str/replace "123abc" #"^\d+" "")
; Tulostaa "abc"

; Poistetaan kaikki merkit, jotka eivät ole numeroita tai kirjaimia
(str/replace "12#%3@$" #"[^a-zA-Z\d]" "")
; Tulostaa "123"

```

## Syventävä sukellus
Ennen vanhaan merkkien poistaminen sujui usein käsipelityöllä, mutta nykypäivänä automatisoidut ratkaisut ovat huomattavasti tehokkaampia. Lisäksi on hyvä pitää mielessä, että merkkijonoon ei aina kannata tehdä muutoksia suoraan, vaan joskus parempi ratkaisu voi olla esimerkiksi uuden merkkijonon luominen.

## Katso myös
[Lähdekoodi](https://github.com/clojure/clojure-contrib/blob/master/src/main/clojure/clojure/contrib/string.clj) | [Dokumentaatio](https://clojure.github.io/clojure-contrib/string-api.html) | [Regex-ilmaisukirjasto](https://clojure.org/reference/java_interop#_javautilregex)